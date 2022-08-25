;;; build-rules.el --- Build rules for barberry.io -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.1
;; Package-Requires: ((emacs "29") (publicatorg "0.1"))
;;
;; Created: 23 Jun 2022
;;
;; URL: https://github.com/d12frosted/barberry.io
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require 'init)

(require 'dash)
(require 'vulpea)
(require 'vino)
(require 'publicatorg)



(defconst brb-supported-images '("jpeg" "png" "jpg" "heic" "webp"))

(defun brb-supported-image-p (file)
  "Return non-nil if FILE is a supported image."
  (seq-contains-p brb-supported-images
                  (s-downcase (file-name-extension file))))



(cl-defmethod porg-describe ((item porg-item))
  "Describe ITEM."
  (pcase (porg-item-type item)
    ("note" (porg-describe (porg-item-item item)))
    ("attachment" (concat "(image) " (file-name-nondirectory (porg-item-target-abs item))))
    (_ (concat "(" (porg-item-type item) ") " (porg-item-id item)))))

(cl-defmethod porg-describe ((item porg-rule-output))
  "Describe ITEM."
  (pcase (porg-rule-output-type item)
    ("note" (porg-describe (porg-rule-output-item item)))
    ("attachment" (concat "(image) " (file-name-nondirectory (porg-rule-output-file item))))
    (_ (concat "(" (porg-rule-output-type item) ") " (porg-rule-output-id item)))))

(cl-defmethod porg-describe ((note vulpea-note))
  "Describe NOTE."
  (let ((tags (vulpea-note-tags note)))
    (format "(%s) %s"
            (cond
             ((seq-contains-p tags "producer") "producer")
             ((seq-contains-p tags "cellar") "wine")
             ((seq-contains-p tags "grape") "grape")
             ((seq-contains-p tags "region") "region")
             ((seq-contains-p tags "appellation") "region")
             ((seq-contains-p tags "barberry/post") "post")
             ((seq-contains-p tags "barberry/convive") "convive")
             (t "???"))
            (vulpea-note-title note))))



(cl-defun brb-make-outputs (&key file
                                 attach-dir
                                 attach-filter
                                 soft-deps
                                 hard-deps
                                 outputs-extra)
  "Make outputs function for note.

Just a wrapper around `porg-note-output' and
`porg-attachments-output'.

FILE is a function that takes a `vulpea-note' and returns
relative output file.

ATTACH-DIR is a function that takes a `porg-rule-output' of note
and returns relative directory for attachments. Optional. I am
too lazy to explain default implementation.

ATTACH-FILTER is a predicate on attachment file. Controls which
attachments should be part of the output. Defaults to
`brb-supported-image-p'.

OUTPUTS-EXTRA is a function that takes a `porg-rule-output' of
note and returns list of additional outputs.

See `porg-note-output' for documentation for SOFT-DEPS and
HARD-DEPS. But in this case these are functions on
`vulpea-note'."
  (lambda (note)
    (let ((note-output
           (porg-note-output note
                             :file (funcall file note)
                             :soft-deps (when soft-deps (funcall soft-deps note))
                             :hard-deps (when hard-deps (funcall hard-deps note)))))
      (-concat (list note-output)
               (porg-attachments-output
                note
                :dir (if attach-dir
                         (funcall attach-dir note-output)
                       (let ((name (directory-from-uuid
                                    (file-name-base (porg-rule-output-file note-output)))))
                         (concat "images/" name)))
                :file-mod #'file-name-fix-attachment
                :filter (or attach-filter #'brb-supported-image-p))
               (when outputs-extra
                 (funcall outputs-extra note-output))))))

(cl-defun brb-make-publish (&key copy-fn metadata)
  "Create public function with COPY-FN and METADATA."
  (lambda (item items _cache)
    (let* ((target (porg-item-target-abs item))
           (hash-a (when (file-exists-p target)
                     (porg-sha1sum (porg-item-target-abs item)))))
      ;; 1. copy file
      (mkdir (file-name-directory target) 'parents)
      (funcall copy-fn item items)

      ;; 2. remove private parts
      (porg-clean-noexport-headings target)

      ;; 3. cleanup and transform links
      (with-current-buffer (find-file-noselect target)
        (porg-clean-links-in-buffer
         :sanitize-id-fn (-rpartial #'brb-sanitize-id-link items)
         :sanitize-attachment-fn
         (lambda (link)
           (let* ((path (org-ml-get-property :path link))
                  (path (file-name-fix-attachment path))
                  (dir (directory-from-uuid (file-name-base target))))
             (->> link
                  (org-ml-set-property :path (format "/images/%s/%s" dir path))
                  (org-ml-set-property :type "file")
                  (org-ml-set-children nil)))))
        (save-buffer))

      ;; 4. generate metadata
      (let* ((meta-file (concat (porg-item-target-abs item) ".metadata"))
             (note (porg-item-item item))

             (hash-b (porg-sha1sum (porg-item-target-abs item)))

             (update (when (file-exists-p meta-file)
                       (with-current-buffer (find-file-noselect meta-file)
                         (goto-char (point-min))
                         (when (re-search-forward "update: \"\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\"" nil t)
                           (match-string 1)))))
             (update (or (and (string-equal hash-a hash-b) update)
                         (format-time-string "%F")))

             (publish (or (vulpea-note-meta-get note "publish") "false"))
             (hide (or (vulpea-note-meta-get note "hide") "false"))

             (meta (if (functionp metadata) (funcall metadata item) metadata))
             (meta (-concat (list "publish" publish
                                  "hide" hide
                                  "title" (vulpea-note-title note))
                            (when update (list "update" update))
                            meta)))
        (with-current-buffer (find-file-noselect meta-file)
          (delete-region (point-min) (point-max))
          (cl-loop for (key value) on meta by 'cddr
                   do (insert key ": " "\"" (string-from value) "\"" "\n"))
          (save-buffer))))))



(cl-defun brb-build-wine (item items)
  "Copy wine ITEM.

Access to full ITEMS for related wines."
  (with-current-buffer (find-file-noselect (porg-item-target-abs item))
    (delete-region (point-min) (point-max))
    (let* ((note (porg-item-item item))
           (colour (vulpea-note-meta-get note "colour"))
           (carbonation (vulpea-note-meta-get note "carbonation"))
           (sweetness (vulpea-note-meta-get note "sweetness"))
           (roa (or (vulpea-note-meta-get note "region" 'note)
                    (vulpea-note-meta-get note "appellation" 'note)))
           (country (vulpea-note-meta-get roa "country" 'note))
           (producer (vulpea-note-meta-get note "producer" 'note))
           (grapes (vulpea-note-meta-get-list note "grapes" 'note))
           (vintage (or (vulpea-note-meta-get note "vintage") "NV"))
           (alcohol (vulpea-note-meta-get note "alcohol"))
           (sugar (or (vulpea-note-meta-get note "sugar") "NA"))
           (prices (vulpea-note-meta-get-list note "price"))
           (available (vulpea-note-meta-get note "available" 'number))
           (ratings (vulpea-note-meta-get-list note "ratings" 'note))
           (related (brb-public-items (brb-related-wines note) items))
           (images (vulpea-note-meta-get-list note "images")))
      (insert
       "#+attr_html: :class wine-main-image\n"
       (if images
           (car images)
         "[[file:/images/unknown-wine.webp]]")
       "\n\n"
       "- Type :: "
       (s-capitalize colour)
       " "
       (s-capitalize carbonation)
       ", "
       (s-capitalize (s-replace "-" " " sweetness))
       "\n"

       "- Producer :: " (vulpea-utils-link-make-string producer) "\n"
       "- Vintage :: " vintage "\n"

       "- Location :: "
       (vulpea-note-title country)
       (if (string-equal (vulpea-note-title roa) (vulpea-note-title country))
           ""
         (concat
          ", "
          (vulpea-note-title roa)))
       "\n"

       "- Grapes :: " (mapconcat #'vulpea-note-title grapes ", ") "\n"
       "- Alcohol :: " alcohol "\n"
       "- Sugar :: " sugar "\n"
       "- Price :: " (mapconcat #'identity prices ", ") "\n"
       "- Cellar :: "
       (cond
        ((= available 1) "1 bottle")
        ((> available 1) (format "%i bottles" available))
        (t "N/A"))
       "\n"

       "\n"

       (vulpea-utils-with-note note
         (let* ((meta (vulpea-buffer-meta))
                (pl (plist-get meta :pl)))
           (buffer-substring-no-properties
            (org-element-property :end pl)
            (point-max))))
       "\n")

      (let* ((full)
             (content
              (vulpea-utils-with-note producer
                (let* ((meta (vulpea-buffer-meta))
                       (pl (plist-get meta :pl))
                       (p0 (if pl
                               (org-element-property :end pl)
                             (save-excursion
                               (goto-char (point-min))
                               (while (looking-at org-property-re)
                                 (forward-line 1))
                               (while (looking-at "^#\\+.+$")
                                 (forward-line 1))
                               (while (and (not (eobp)) (looking-at "^ *$"))
                                 (forward-line 1))
                               (point))))
                       (p1 (or
                            (org-element-map (org-element-parse-buffer 'headline)
                                'headline
                              (lambda (hl)
                                (org-element-property :begin hl))
                              nil 'first-match)
                            (point-max))))
                  (setq full (= p1 (point-max)))
                  (buffer-substring-no-properties p0 p1)))))
        (unless (string-empty-p (s-trim content))
          (insert "* Producer\n\n"  content "\n")
          (unless full
            (insert (org-link-make-string (concat "id:" (vulpea-note-id producer))
                                          "Read more...")
                    "\n\n"))))

      (insert "* Ratings\n\n")
      (if ratings
          (seq-each
           (lambda (rating)
             (let ((date (vulpea-note-meta-get rating "date"))
                   (rate (vulpea-note-meta-get rating "total" 'number)))
               (insert
                "** " date " - ☆ " (format "%.2f" rate) "\n\n"
                (vulpea-utils-with-note rating
                  (let* ((meta (vulpea-buffer-meta))
                         (pl (plist-get meta :pl)))
                    (buffer-substring-no-properties
                     (org-element-property :end pl)
                     (point-max))))
                "\n")))
           ratings)
        (insert "There are no ratings of this wine yet. It's waiting for the right moment, which could be today, tomorrow or even in a year. Or maybe, I am drinking it at this moment... So stay tuned!\n\n"))

      (when related
        (insert "* Related\n\n")
        (insert
         "#+begin_export html\n"
         "<div class=\"flex-container\">\n")
        (--each-indexed related
          (unless (porg-item-item it)
            (user-error "Could not get related note of '%s'" (vulpea-note-title note)))
          (let* ((note (porg-item-item it))
                 (id (vulpea-note-id note))
                 (img (vulpea-note-meta-get note "images" 'link))
                 (img-item (when img
                             (gethash (concat id ":" (s-chop-prefix "attachment:" img)) items)))
                 (producer (vulpea-note-meta-get note "producer" 'note))
                 (name (vulpea-note-meta-get note "name"))
                 (vintage (or (vulpea-note-meta-get note "vintage") "NV"))
                 (pos (if (= (mod it-index 2) 0)
                          "flex-item-left"
                        "flex-item-right")))
            (insert
             "  <a class=\"flex-item " pos "\" href=\"/wines/" id ".html\">\n"
             (concat "    <img class=\"flex-bottle\" src=\"/"
                     (if img-item
                         (porg-item-target-rel img-item)
                       "images/unknown-wine.webp")
                     "\"></img>\n")
             "    <section class=\"h text-small text-lighter\">" (vulpea-note-title producer) "</section>\n"
             "    <section class=\"h text-bolder\">" name " - " vintage "</section>\n"
             "  </a>\n\n")))
        (insert
         "</div>\n"
         "#+end_export\n"))

      (goto-char (point-min))
      (while (re-search-forward "\\(\\*\\{1,\\}\\)" nil 'noerror)
        (replace-match "*\\1"))
      (goto-char (point-max))

      (goto-char (point-min))
      (while (re-search-forward "\n\\{3,\\}" nil 'noerror)
        (replace-match "\n\n")))
    (save-buffer)))



(cl-defun brb-build-producer (item items)
  "Build producer ITEM.

Access to full ITEMS for related wines."
  (let ((wines (brb-public-items (brb-wines-by-producer (porg-item-item item)) items)))
    (with-current-buffer (find-file-noselect (porg-item-target-abs item))
      (delete-region (point-min) (point-max))
      (insert
       (vulpea-utils-with-note (porg-item-item item)
         (let* ((meta (vulpea-buffer-meta))
                (pl (plist-get meta :pl)))
           (buffer-substring-no-properties
            (if pl
                (org-element-property :end pl)
              (save-excursion
                (goto-char (point-min))
                (while (looking-at org-property-re)
                  (forward-line 1))
                (while (looking-at "^#\\+.+$")
                  (forward-line 1))
                (while (and (not (eobp)) (looking-at "^ *$"))
                  (forward-line 1))
                (point)))
            (point-max))))
       "\n"
       "* Wines\n\n")
      (if wines
          (progn
            (insert
             "#+begin_export html\n"
             "<div class=\"flex-container\">\n")
            (--each-indexed (brb-wines-sort (-map #'porg-item-item wines))
              (let* ((id (vulpea-note-id it))
                     (img (vulpea-note-meta-get it "images" 'link))
                     (img-item (when img
                                 (gethash (concat id ":" (s-chop-prefix "attachment:" img)) items)))
                     (name (vulpea-note-meta-get it "name"))
                     (vintage (or (vulpea-note-meta-get it "vintage") "NV"))
                     (pos (if (= (mod it-index 2) 0)
                              "flex-item-left"
                            "flex-item-right")))
                (insert
                 "  <a class=\"flex-item " pos "\" href=\"/wines/" id ".html\">\n"
                 (concat "    <img class=\"flex-bottle\" src=\"/"
                         (if img-item
                             (porg-item-target-rel img-item)
                           "images/unknown-wine.webp")
                         "\"></img>\n")

                 "    <section class=\"h text-small text-lighter\">★ "
                 (if (vulpea-note-meta-get it "ratings")
                     (format "%.2f" (vulpea-note-meta-get it "rating" 'number))
                   "-")
                 "</section>\n"
                 "    <section class=\"h text-bolder\">" name " - " vintage "</section>\n"
                 "  </a>\n\n")))
            (insert
             "</div>\n"
             "#+end_export\n"))
        (insert "No wines of this producer are present on this site. How did you find this page?"))

      (goto-char (point-min))
      (while (re-search-forward "\\(\\*\\{1,\\}\\)" nil 'noerror)
        (replace-match "*\\1"))
      (goto-char (point-max))

      (goto-char (point-min))
      (while (re-search-forward "\n\\{3,\\}" nil 'noerror)
        (replace-match "\n\n"))

      (save-buffer))))



(cl-defun brb-build-post (item _items)
  "Build post ITEM."
  (vulpea-utils-with-note (porg-item-item item)
    (--each (seq-reverse
             (org-element-map
                 (org-element-parse-buffer 'element)
                 'src-block
               (lambda (h)
                 (org-element-property :begin h))))
      (goto-char it)
      (let ((org-confirm-babel-evaluate nil))
        (save-excursion
          (silenzio
           (org-babel-execute-src-block)))))
    (save-buffer))
  (with-current-buffer (find-file-noselect (porg-item-target-abs item))
    (delete-region (point-min) (point-max))
    (insert (vulpea-utils-with-note (porg-item-item item)
              (let* ((meta (vulpea-buffer-meta))
                     (pl (plist-get meta :pl)))
                (buffer-substring-no-properties
                 (if pl
                     (org-element-property :end pl)
                   (save-excursion
                     (goto-char (point-min))
                     (while (looking-at org-property-re)
                       (forward-line 1))
                     (while (looking-at "^#\\+.+$")
                       (forward-line 1))
                     (while (looking-at "^ *$")
                       (forward-line 1))
                     (point)))
                 (point-max)))))
    (save-buffer)))



(cl-defun brb-build-convive (item items)
  "Build convive ITEM.

Access to full ITEMS for related wines."
  (let* ((ratings-all (->> item
                           (porg-item-item)
                           (vulpea-note-id)
                           (cons "id")
                           (list)
                           (vulpea-db-query-by-links-every)
                           (--filter (vulpea-note-tagged-all-p it "wine" "rating"))))
         (wines (brb-public-items (--map (vulpea-note-meta-get it "wine" 'note) ratings-all) items))
         (wines-tbl (let ((tbl (make-hash-table :test 'equal :size (seq-length wines))))
                      (--each (-map #'porg-item-item wines)
                        (puthash (vulpea-note-id it) it tbl))
                      tbl))
         (ratings (->> ratings-all
                       (--filter (gethash (vulpea-note-id (vulpea-note-meta-get it "wine" 'note))
                                          wines-tbl))
                       (seq-sort-by (lambda (note)
                                      (vulpea-note-meta-get note "date"))
                                    #'string>))))
    (with-current-buffer (find-file-noselect (porg-item-target-abs item))
      (delete-region (point-min) (point-max))
      (insert "Hello, fellow convive! This page contains a list of wines we tasted together, grouped by date. Some wines that we tasted together are not listed yet, because I haven't published them. Stay tuned!

Bookmark this page and use it for your own good.\n\n")
      (if wines
          (--each (-group-by (lambda (rating) (vulpea-note-meta-get rating "date")) ratings)
            (insert (format "- %s :: \n"
                            (format-time-string "%A, %e %B %Y" (date-to-time (car it)))))
            (--each (cdr it)
              (insert
               "  - "
               (vulpea-utils-link-make-string (vulpea-note-meta-get it "wine" 'note))
               (when-let ((location (vulpea-note-meta-get it "location" 'note)))
                 (concat
                  " "
                  "@@html:<span style=\"color: var(--faded);\">@@"
                  "@" (vulpea-note-title location)
                  "@@html:</span>@@"))
               "\n"))
            (insert "\n"))
        (insert "There are no wines we drunk together. How did you find this page?"))
      (save-buffer))))



(cl-defun brb-publish-wines (target items items-all _cache)
  "Generate wines list from ITEMS and write it to TARGET file.

ITEMS-ALL is input table as returned by `porg-build-input'."
  (with-current-buffer (find-file-noselect target)
    (delete-region (point-min) (point-max))
    (insert
     "#+attr_html: :class wines-table\n"
     (string-table
      :header '("county" "producer" "name" "vintage" "grapes" "rate")
      :header-sep-start "|-" :header-sep "-" :header-sep-conj "-+-" :header-sep-end "-|"
      :row-start "| " :sep " | " :row-end " |"
      :data
      (->> items
           (hash-table-values)
           (--filter (string-equal (porg-item-type it) "note"))
           (-map #'porg-item-item)
           (brb-wines-sort)
           (--map
            (let* ((roa (or (vulpea-note-meta-get it "region" 'note)
                            (vulpea-note-meta-get it "appellation" 'note)))
                   (country (vulpea-note-meta-get roa "country" 'note))
                   (producer (vulpea-note-meta-get it "producer" 'note)))
              (list
               (vulpea-utils-link-make-string country)
               (vulpea-utils-link-make-string producer)
               (org-link-make-string
                (concat "id:" (vulpea-note-id it))
                (vulpea-note-meta-get it "name"))
               (or (vulpea-note-meta-get it "vintage") "NV")
               (mapconcat #'vulpea-utils-link-make-string
                          (vulpea-note-meta-get-list it "grapes" 'note)
                          ", ")
               (if (vulpea-note-meta-get it "ratings")
                   (format "%.2f" (vulpea-note-meta-get it "rating" 'number))
                 "-"))))))
     "\n")
    (porg-clean-links-in-buffer
     :sanitize-id-fn (-rpartial #'brb-sanitize-id-link items-all))
    (save-buffer)))



(cl-defun brb-publish-ratings (target items items-all _cache &optional limit)
  "Generate ratings list from ITEMS in TARGET file.

Ratings are being queried from ITEMS.

Optionally LIMIT ratings, but keep groups full, e.g. LIMIT might
be exceeded.

ITEMS-ALL is input table as returned by `porg-build-input'."
  (let* ((n 0)
         (ratings (->> items
                       (hash-table-values)
                       (--filter (string-equal (porg-item-type it) "note"))
                       (-map #'porg-item-item)
                       (--mapcat (vulpea-note-meta-get-list it "ratings" 'note))
                       (seq-sort-by (lambda (note) (vulpea-note-meta-get note "date")) #'string>)))
         (limit (or limit (seq-length ratings))))
    (with-current-buffer (find-file-noselect target)
      (delete-region (point-min) (point-max))
      (--each (-group-by (lambda (rating) (vulpea-note-meta-get rating "date")) ratings)
        (unless (> n limit)
          (insert (format "- %s :: \n" (format-time-string "%A, %e %B %Y" (date-to-time (car it)))))
          (--each (cdr it)
            (let ((wine (vulpea-note-meta-get it "wine" 'note))
                  (rate (vulpea-note-meta-get it "total" 'number)))
              (insert (format "  - ☆ %.2f - %s\n" rate (vulpea-utils-link-make-string wine)))))
          (insert "\n"))
        (setq n (+ n (- (length it) 1))))
      (porg-clean-links-in-buffer
       :sanitize-id-fn (-rpartial #'brb-sanitize-id-link items-all))
      (save-buffer))))



(cl-defun brb-sanitize-id-link (link items)
  "Sanitize ID LINK according to ITEMS."
  (if-let* ((id (org-ml-get-property :path link))
            (note (vulpea-db-get-by-id id))
            (item (gethash id items))
            (file (porg-item-target-rel item))
            (path (concat "/" (s-chop-suffix ".org" file))))
      (->> link
           (org-ml-set-property :type "barberry")
           (org-ml-set-property :path path))
    (org-ml-from-string
     'plain-text
     (concat (nth 2 link) (s-repeat (or (org-ml-get-property :post-blank link) 0) " ")))))



(cl-defun brb-delete (file)
  "Delete FILE."
  (delete-file file)
  (let ((meta (concat file ".metadata")))
    (delete-file meta)))



(defun file-name-fix-attachment (file-name)
  "Fix attachment FILE-NAME."
  (let* ((ext-old (file-name-extension file-name))
         ;; (ext-new (s-downcase (if (string-equal ext-old "heic") "jpeg" ext-old)))
         (ext-new "webp"))
    (concat (s-replace "_" "-" (s-chop-suffix ext-old file-name)) ext-new)))

(defun directory-from-uuid (uuid)
  "Adapt UUID to directory name."
  (if (string-match string-uuid-regexp uuid)
      (concat (s-left 2 uuid) "/" (s-chop-prefix (s-left 2 uuid) uuid))
    uuid))



(defun brb-wines-sort (wines)
  "Sort WINES."
  (--sort
   (string-lessp (vulpea-note-title it)
                 (vulpea-note-title other))
   wines))



(defun brb-wines-by-producer (producer)
  "List wines by PRODUCER.

PRODUCER is a `vulpea-note'.

Returns list of notes."
  (--> producer
       (vulpea-note-id it)
       (vino-db-query
        [:select [id]
         :from cellar
         :where (= producer $s1)]
        it)
       (-map #'car it)
       (vulpea-db-query-by-ids it)))

(defun brb-related-wines (note)
  "List wines related to wine NOTE."
  (let ((-compare-fn (lambda (a b) (string-equal (vulpea-note-id a) (vulpea-note-id b)))))
    (-distinct (-concat (brb-related-wines-by-producer note)
                        (brb-related-wines-by-date note)))))

(defun brb-related-wines-by-date (note)
  "List wines related to wine NOTE by date."
  (--> note
       (vulpea-note-meta-get-list it "ratings" 'note)
       (--map (vulpea-note-meta-get it "date") it)
       (--mapcat (vino-db-query
                  [:select [wine]
                   :from ratings
                   :where (= date $s1)]
                  it)
                 it)
       (apply #'-concat it)
       (--remove (string-equal it (vulpea-note-id note)) it)
       (vulpea-db-query-by-ids it)))

(defun brb-related-wines-by-producer (note)
  "List wines related to wine NOTE by producer."
  (--> note
       (vulpea-note-meta-get it "producer" 'note)
       (brb-wines-by-producer it)
       (--remove (string-equal (vulpea-note-id it) (vulpea-note-id note)) it)))

(defun brb-public-items (notes items)
  "Exchange list of NOTES to list of ITEMS.

Basically, keep only public notes."
  (->> notes
       (--map (gethash (vulpea-note-id it) items))
       (-filter #'identity)))



(org-link-set-parameters "barberry" :follow #'org-roam-link-follow-link)

(setf porg-log-level 'info)

(porg-define
 :name "barberry.io"
 :root (when load-file-name (file-name-directory load-file-name))
 :cache-file "build-cache"

 :input
 (lambda ()
   (--filter
    (and (null (vulpea-note-primary-title it))
         (= 0 (vulpea-note-level it)))
    (vulpea-db-query-by-tags-every '("barberry/public"))))

 :rules
 (list
  (porg-rule
   :name "wines"
   :match (-rpartial #'vulpea-note-tagged-all-p "wine" "cellar")
   :outputs
   (brb-make-outputs
    :file (lambda (note) (concat "wines/" (vulpea-note-id note) ".org"))
    :hard-deps (lambda (note)
                 (-concat
                  (list (vulpea-note-meta-get note "producer" 'note))
                  (vulpea-note-meta-get-list note "ratings" 'note)))
    :soft-deps #'brb-related-wines
    ;; TODO: consider providing a separate key for extra attachments
    :outputs-extra (lambda (output)
                     (let* ((note (porg-rule-output-item output))
                            (producer (vulpea-note-meta-get note "producer" 'note))
                            (ratings (vulpea-note-meta-get-list note "ratings" 'note)))
                       (-concat
                        (porg-attachments-output
                         producer
                         :dir (let ((name (directory-from-uuid (file-name-base (porg-rule-output-file output)))))
                                (concat "images/" name))
                         :file-mod #'file-name-fix-attachment
                         :filter #'brb-supported-image-p
                         :owner note)
                        (-flatten
                         (--map
                          (porg-attachments-output
                           it
                           :dir (let ((name (directory-from-uuid (file-name-base (porg-rule-output-file output)))))
                                  (concat "images/" name))
                           :file-mod #'file-name-fix-attachment
                           :filter #'brb-supported-image-p
                           :owner note)
                          ratings))
                        (list
                         (porg-rule-output
                          :id (concat (vulpea-note-id note) ".json")
                          :type "json"
                          :item note
                          :file (concat "wines/" (vulpea-note-id note) ".json")
                          :hard-deps (porg-rule-output-hard-deps output)
                          :soft-deps (porg-rule-output-soft-deps output))))))))
  
  (porg-rule
   :name "producers"
   :match (-rpartial #'vulpea-note-tagged-all-p "wine" "producer")
   :outputs
   (brb-make-outputs
    :file (lambda (note) (concat "producers/" (vulpea-note-id note) ".org"))
    :soft-deps (lambda (note) (brb-wines-by-producer note))))

  (porg-rule
   :name "ratings"
   :match (-rpartial #'vulpea-note-tagged-all-p "wine" "rating")
   :outputs (lambda (note) (list (porg-void-output note))))

  (porg-rule
   :name "grapes"
   :match (-rpartial #'vulpea-note-tagged-all-p "wine" "grape")
   :outputs nil)

  (porg-rule
   :name "posts"
   :match (-rpartial #'vulpea-note-tagged-all-p "barberry/post")
   :outputs
   (brb-make-outputs
    :file (lambda (note)
            (concat
             "posts/"
             (vulpea-utils-with-note note
               (let ((date (vulpea-buffer-prop-get "date"))
                     (slug (or (vulpea-buffer-prop-get "slug")
                               (porg-slug (vulpea-note-title note)))))
                 (unless date (user-error "Post '%s' is missing date" (vulpea-note-title note)))
                 (concat (org-read-date nil nil date) "-" slug)))
             ".org"))))

  (porg-rule
   :name "convives"
   :match (-rpartial #'vulpea-note-tagged-all-p "barberry/convive")
   :outputs (lambda (note)
              (list
               (porg-note-output
                note
                :file (concat "convives/" (vulpea-note-id note) ".org")
                :soft-deps
                (->> (vulpea-note-id note)
                     (cons "id")
                     (list)
                     (vulpea-db-query-by-links-every)
                     (--filter (vulpea-note-tagged-all-p it "wine" "rating")))))))

  (porg-batch-rule
   :name "wines"
   :filter (-rpartial #'porg-item-that :type "note"
                      :predicate (-rpartial #'vulpea-note-tagged-all-p "wine" "cellar"))
   :target "pages/wines.org"
   :publish #'brb-publish-wines)

  (porg-batch-rule
   :name "reviews"
   :filter (-rpartial #'porg-item-that :type "note"
                      :predicate (-rpartial #'vulpea-note-tagged-all-p "wine" "cellar"))
   :target "pages/reviews.org"
   :publish #'brb-publish-ratings)

  (porg-batch-rule
   :name "latest reviews"
   :filter (-rpartial #'porg-item-that :type "note"
                      :predicate (-rpartial #'vulpea-note-tagged-all-p "wine" "cellar"))
   :target "pages/reviews-latest.org"
   :publish (-rpartial #'brb-publish-ratings 12)))

 :compilers
 (list
  (porg-compiler
   :name "wine"
   :match (-rpartial #'porg-rule-output-that :type "note"
                     :predicate (-rpartial #'vulpea-note-tagged-all-p "wine" "cellar"))
   :hash #'porg-sha1sum
   :build
   (brb-make-publish
    :copy-fn #'brb-build-wine
    :metadata
    (lambda (item)
      (let* ((note (porg-item-item item))
             (rating (vulpea-note-meta-get note "rating"))
             (rating (unless (string-equal rating "NA") (string-to-number rating))))

        (-concat
         (list "producer" (vulpea-note-meta-get note "producer" 'note)
               "name" (vulpea-note-meta-get note "name")
               "vintage" (or (vulpea-note-meta-get note "vintage") "NV")
               "country" (vulpea-note-meta-get
                          (or (vulpea-note-meta-get note "region" 'note)
                              (vulpea-note-meta-get note "appellation" 'note))
                          "country"
                          'note)
               "grapes" (mapconcat
                         #'vulpea-note-title (vulpea-note-meta-get-list note "grapes" 'note) ", "))
         (when rating (list "rating" (format "%.2f" rating)))))))
   :clean #'brb-delete)

  (porg-compiler
   :name "wine-json"
   :match (-rpartial #'porg-rule-output-that :type "json"
                     :predicate (-rpartial #'vulpea-note-tagged-all-p "wine" "cellar"))
   :hash #'porg-sha1sum
   :build
   (lambda (item items _cache)
     (let* ((note (porg-item-item item))
            (colour (vulpea-note-meta-get note "colour"))
            (carbonation (vulpea-note-meta-get note "carbonation"))
            (sweetness (vulpea-note-meta-get note "sweetness"))
            (roa (or (vulpea-note-meta-get note "region" 'note)
                     (vulpea-note-meta-get note "appellation" 'note)))
            (country (vulpea-note-meta-get roa "country" 'note))
            (producer (vulpea-note-meta-get note "producer" 'note))
            (grapes (vulpea-note-meta-get-list note "grapes" 'note))
            (vintage (or (vulpea-note-meta-get note "vintage") "NV"))
            (alcohol (vulpea-note-meta-get note "alcohol"))
            (sugar (or (vulpea-note-meta-get note "sugar") "NA"))
            (prices (vulpea-note-meta-get-list note "price"))
            (available (vulpea-note-meta-get note "available" 'number))
            (rating (vulpea-note-meta-get note "rating"))
            (images (vulpea-note-meta-get-list note "images" 'link))
            (image (when images (car images)))
            (image (when image (s-chop-prefix "attachment:" image)))
            (image (when image (gethash (concat (vulpea-note-id note) ":" image) items)))
            (json-encoding-pretty-print t))
       (with-current-buffer (find-file-noselect (porg-item-target-abs item))
         (delete-region (point-min) (point-max))
         (insert
          (json-encode
           (list
            :id (vulpea-note-id note)
            :name (vulpea-note-meta-get note "name")
            :full-name (vulpea-note-title note)
            :producer (list :id (vulpea-note-id producer)
                            :name (vulpea-note-title producer))
            :vintage vintage
            :colour colour
            :carbonation carbonation
            :sweetness sweetness
            :country (list :id (vulpea-note-id country)
                           :name (vulpea-note-title country))
            :grapes (apply #'vector
                           (--map (list :id (vulpea-note-id it)
                                        :name (vulpea-note-title it))
                                  grapes))
            :alcohol alcohol
            :sugar sugar
            :prices prices
            :available available
            :image (if image
                       (porg-item-target-rel image)
                     "images/unknown-wine.webp")
            :rating rating)))
         (save-buffer))))
   :clean #'brb-delete)

  (porg-compiler
   :name "producer"
   :match (-rpartial #'porg-rule-output-that :type "note"
                     :predicate (-rpartial #'vulpea-note-tagged-all-p "wine" "producer"))
   :hash #'porg-sha1sum
   :build
   (brb-make-publish
    :copy-fn #'brb-build-producer
    :metadata (lambda (item)
                (when-let* ((note (porg-item-item item))
                            (meta-file (concat (porg-item-target-abs item) ".metadata"))
                            (publish (vulpea-note-meta-get note "publish"))
                            (publish (string-equal publish "true")))
                  (vulpea-utils-with-note note
                    (let ((date (vulpea-buffer-prop-get "date"))
                          (language (vulpea-buffer-prop-get "language"))
                          (tags (vulpea-buffer-prop-get-list "tags")))
                      (unless date (user-error "Post '%s' is missing date" (vulpea-note-title note)))
                      (unless language (user-error "Producer '%s' is missing language" (vulpea-note-title note)))
                      (list
                       "date" (org-read-date nil nil date)
                       "language" language
                       "tags" (string-join (-distinct (-concat '("producer") tags)) ", ")))))))
   :clean #'brb-delete)

  (porg-compiler
   :name "post"
   :match (-rpartial #'porg-rule-output-that :type "note"
                     :predicate (-rpartial #'vulpea-note-tagged-all-p "barberry/post"))
   :hash #'porg-sha1sum
   :build
   (brb-make-publish
    :copy-fn #'brb-build-post
    :metadata
    (lambda (item)
      (let ((note (porg-item-item item)))
        (vulpea-utils-with-note note
          (let ((date (vulpea-buffer-prop-get "date"))
                (language (vulpea-buffer-prop-get "language"))
                (author (vulpea-buffer-prop-get "author"))
                (image (vulpea-buffer-prop-get "image"))
                (description (vulpea-buffer-prop-get "description"))
                (tags (vulpea-buffer-prop-get-list "tags")))
            (unless date (user-error "Post '%s' is missing date" (vulpea-note-title note)))
            (unless language (user-error "Post '%s' is missing language" (vulpea-note-title note)))
            (unless author (user-error "Post '%s' is missing author" (vulpea-note-title note)))
            (-concat
             (list "date" (org-read-date nil nil date)
                   "language" language
                   "author" author)
             (when image
               (list "image"
                     (concat
                      (file-name-as-directory
                       (concat "images/" (file-name-base (porg-item-target-rel item))))
                      (file-name-fix-attachment image))))
             (when description
               (list "description" description))
             (when tags
               (list "tags" (string-join tags ", ")))))))))
   :clean #'brb-delete)

  (porg-compiler
   :name "convive"
   :match (-rpartial #'porg-rule-output-that :type "note"
                     :predicate (-rpartial #'vulpea-note-tagged-all-p "barberry/convive"))
   :hash #'porg-sha1sum
   :build (brb-make-publish :copy-fn #'brb-build-convive)
   :clean #'brb-delete)

  (porg-compiler
   :name "images"
   :match (-rpartial #'porg-rule-output-that :type "attachment" :predicate #'brb-supported-image-p)
   :build
   (lambda (item _items _cache)
     (let ((max-width 960)
           (width (string-to-number
                   (shell-command-to-string
                    (format "identify -format %%W '%s'" (porg-item-item item))))))
       (make-directory (file-name-directory (porg-item-target-abs item)) 'parents)
       (porg-debug "input:  %s" (porg-item-item item))
       (porg-debug "output: %s" (porg-item-target-abs item))
       (if (> width max-width)
           (shell-command-to-string
            (format "convert '%s' -strip -auto-orient -resize %sx100^ '%s'"
                    (porg-item-item item) max-width (porg-item-target-abs item)))
         (shell-command-to-string
          (format "convert '%s' -strip -auto-orient '%s'"
                  (porg-item-item item) (porg-item-target-abs item))))))
   :clean #'brb-delete)))



(provide 'build-rules)
;;; build-rules.el ends here
