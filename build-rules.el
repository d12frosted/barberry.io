;;; build-rules.el --- Build rules for barberry.io -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
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

(require 'dash)
(require 'vulpea)

(require 'lib-string)
(require 'lib-publicatorg)
(require 'lib-fun)

(org-link-set-parameters "barberry" :follow #'org-roam-link-follow-link)

(porg-define
 :name "barberry.io"
 :root (when load-file-name
         (file-name-directory load-file-name))
 :cache-file "build-cache.el"
 :input
 (lambda ()
   (--filter
    (and (null (vulpea-note-primary-title it))
         (= 0 (vulpea-note-level it)))
    (vulpea-db-query-by-tags-every '("barberry/public"))))
 :describe
 (lambda (note)
   (let* ((note (if (listp note) (plist-get note :note) note))
          (tags (vulpea-note-tags note)))
     (format "(%s) %s"
             (cond
              ((seq-contains-p tags "producer") "producer")
              ((seq-contains-p tags "cellar") "wine")
              ((seq-contains-p tags "grape") "grape")
              ((seq-contains-p tags "region") "region")
              ((seq-contains-p tags "appellation") "region")
              ((seq-contains-p tags "barberry/post") "post")
              (t "???"))
             (vulpea-note-title note))))

 (porg-rule
  :name "wines"
  :match (-rpartial #'vulpea-note-tagged-all-p "wine" "cellar")
  :dependencies (lambda (note)
                  (list (vulpea-note-meta-get note "producer" 'note)))
  :soft-dependencies (lambda (note)
                       (-concat
                        (vulpea-note-meta-get-list note "ratings" 'note)
                        (brb-related-wines note)))
  :target (lambda (note)
            (expand-file-name
             (concat "wines/" (vulpea-note-id note) ".org")))
  :publish
  (lambda (piece input cache)
    (brb-publish
     piece input cache
     :copy-fn (-partial #'brb-copy-wine input)
     :metadata (let* ((note (plist-get piece :note))
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
                        "grapes" (mapconcat #'vulpea-note-title (vulpea-note-meta-get-list note "grapes" 'note) ", "))
                  (when rating (list "rating" (format "%.2f" rating)))))))
  :clean #'brb-delete)

 (porg-rule
  :name "producers"
  :match (-rpartial #'vulpea-note-tagged-all-p "wine" "producer")
  :soft-dependencies (lambda (note) (brb-wines-by-producer note))
  :target (lambda (note)
            (expand-file-name
             (concat "producers/" (vulpea-note-id note) ".org")))
  :publish
  (lambda (piece input cache)
    (brb-publish
     piece input cache
     :copy-fn (-partial #'brb-copy-producer input)
     :metadata nil)))

 (porg-rule
  :name "posts"
  :match (-rpartial #'vulpea-note-tagged-all-p "barberry/post")
  :target (lambda (note)
            (expand-file-name
             (concat
              "posts/"
              (vulpea-utils-with-note note
                (let ((date (vulpea-buffer-prop-get "date"))
                      (slug (or (vulpea-buffer-prop-get "slug")
                                (porg-slug (vulpea-note-title note)))))
                  (unless date (user-error "Post '%s' is missing date" (vulpea-note-title note)))
                  (concat (org-read-date nil nil date) "-" slug)))
              ".org")))
  :publish
  (lambda (piece input cache)
    (brb-publish
     piece input cache
     :copy-fn #'brb-copy-post
     :metadata
     (let ((note (plist-get piece :note)))
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
                      (concat "images/" (file-name-base (plist-get piece :target))))
                     (file-name-fix-attachment image))))
            (when description
              (list "description" description))
            (when tags
              (list "tags" (string-join tags ", ")))))))))
  :clean #'brb-delete)

 (porg-batch-rule
  :name "reviews"
  :filter (-rpartial #'vulpea-note-tagged-all-p "wine" "cellar")
  :target "pages/reviews.org"
  :publish #'brb-publish-ratings)

 (porg-batch-rule
  :name "latest reviews"
  :filter (-rpartial #'vulpea-note-tagged-all-p "wine" "cellar")
  :target "pages/reviews-latest.org"
  :publish (-rpartial #'brb-publish-ratings 12)))



(cl-defun brb-publish (piece input cache &key copy-fn metadata)
  "Publish PIECE with extra METADATA.

INPUT is produced by `porg-build-input'.

CACHE is the build cache.

COPY-FN is used for `porg-copy-note'."
  (let ((note (plist-get piece :note))
        (target (plist-get piece :target)))
    ;; 1. copy file
    (porg-copy-note note target :copy-fn copy-fn)

    ;; 2. copy images
    (brb-copy-images piece cache)

    ;; 3. remove private parts
    (porg-clean-noexport-headings target)

    ;; 4. cleanup and transform links
    (with-current-buffer (find-file-noselect target)
      (porg-clean-links-in-buffer
       :sanitize-id-fn (-partial #'brb-sanitize-id-link input)
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

    ;; 5. generate metadata
    (brb-make-meta-file piece target cache metadata)))



(cl-defun brb-copy-wine (input note path)
  "Copy wine NOTE to PATH.

Access to full INPUT for related wines."
  (with-current-buffer (find-file-noselect path)
    (delete-region (point-min) (point-max))
    (let* ((colour (vulpea-note-meta-get note "colour"))
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
           (related (brb-public-notes (brb-related-wines note) input))
           (images (vulpea-note-meta-get-list note "images")))
      (insert
       (if images
           (concat
            "#+attr_html: :class wine-main-image\n"
            (car images)
            "\n\n")
         "")
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
          (unless (plist-get it :note)
            (user-error "Could not get related note of '%s'" (vulpea-note-title note)))
          (let* ((note (plist-get it :note))
                 (id (vulpea-note-id note))
                 (producer (vulpea-note-meta-get note "producer" 'note))
                 (name (vulpea-note-meta-get note "name"))
                 (vintage (or (vulpea-note-meta-get note "vintage") "NV"))
                 (pos (if (= (mod it-index 2) 0)
                          "flex-item-left"
                        "flex-item-right")))
            (insert
             "  <a class=\"flex-item " pos "\" href=\"/wines/" id ".html\">\n"
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



(cl-defun brb-copy-producer (input note path)
  "Copy producer NOTE to PATH.

Access to full INPUT for related wines."
  (let ((wines (brb-public-notes (brb-wines-by-producer note) input)))
    (with-current-buffer (find-file-noselect path)
      (delete-region (point-min) (point-max))
      (insert
       (vulpea-utils-with-note note
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
          (insert
           "#+attr_html: :class wines-table\n"
           (string-table
            :header '("name" "vintage" "grapes" "region" "rating")
            :header-sep "-"
            :header-sep-start "|-"
            :header-sep-conj "-+-"
            :header-sep-end "-|"
            :row-start "| "
            :row-end " |"
            :sep " | "
            :data
            (--map
             (let* ((note (plist-get it :note))
                    (roa (or (vulpea-note-meta-get note "region" 'note)
                             (vulpea-note-meta-get note "appellation" 'note))))
               (list (org-link-make-string
                      (concat "id:" (vulpea-note-id note))
                      (vulpea-note-meta-get note "name"))
                     (or (vulpea-note-meta-get note "vintage") "NV")
                     (mapconcat #'vulpea-note-title
                                (vulpea-note-meta-get-list note "grapes" 'note)
                                ", ")
                     (vulpea-note-title roa)
                     (if (vulpea-note-meta-get note "ratings")
                         (format "%.2f" (vulpea-note-meta-get note "rating" 'number))
                       "-")))
             wines))
           "\n")
        (insert "No wines of this producer are present on this site. How did you find this page?"))

      (goto-char (point-min))
      (while (re-search-forward "\\(\\*\\{1,\\}\\)" nil 'noerror)
        (replace-match "*\\1"))
      (goto-char (point-max))

      (goto-char (point-min))
      (while (re-search-forward "\n\\{3,\\}" nil 'noerror)
        (replace-match "\n\n"))

      (save-buffer))))



(cl-defun brb-copy-post (note path)
  "Copy post NOTE to PATH."
  (vulpea-utils-with-note note
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
  (with-current-buffer (find-file-noselect path)
    (delete-region (point-min) (point-max))
    (insert (vulpea-utils-with-note note
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



(cl-defun brb-publish-ratings (notes target input _cache &optional limit)
  "Generate ratings list in TARGET file.

RATINGS are being queried from NOTES (which are cellar notes).

INPUT is input table as returned by `porg-build-input'.

Optionally LIMIT ratings, but keep groups full, e.g. LIMIT might
be exceeded."
  (let* ((n 0)
         (ratings (->> notes
                       (--mapcat (vulpea-note-meta-get-list it "ratings" 'note))
                       (seq-sort-by (lambda (note)
                                      (vulpea-note-meta-get note "date"))
                                    #'string>)))
         (limit (or limit (seq-length ratings))))
    (with-current-buffer (find-file-noselect target)
      (delete-region (point-min) (point-max))
      (--each (-group-by (lambda (rating) (vulpea-note-meta-get rating "date")) ratings)
        (unless (> n limit)
          (insert (format "- %s :: \n"
                          (format-time-string "%A, %e %B %Y" (date-to-time (car it)))))
          (--each (cdr it)
            (insert "  - " (brb-rating-to-string it) "\n"))
          (insert "\n"))
        (setq n (+ n (- (length it) 1))))
      (porg-clean-links-in-buffer
       :sanitize-id-fn (-partial #'brb-sanitize-id-link input))
      (save-buffer))))

(cl-defun brb-rating-to-string (rating)
  "Convert RATING to string."
  (let ((wine (vulpea-note-meta-get rating "wine" 'note))
        (rate (vulpea-note-meta-get rating "total" 'number)))
    (format "☆ %.2f - %s" rate (vulpea-utils-link-make-string wine))))



(cl-defun brb-common-metadata (piece cache)
  "Extract common metadata from PIECE and CACHE."
  (let* ((note (plist-get piece :note))
         (target-hash-a (plist-get piece :target-hash))
         (target-hash-b (porg-sha1sum (plist-get piece :target)))
         (update (if (string-equal target-hash-a target-hash-b)
                     (or (porg-cache-get (vulpea-note-id note) :update cache)
                         (format-time-string "%F"))
                   (format-time-string "%F")))
         (publish (vulpea-utils-with-note note
                    (or (vulpea-buffer-prop-get "publish") "true"))))
    (-concat
     (list "publish" publish
           "title" (vulpea-note-title note))
     (when update (list "update" update)))))

(cl-defun brb-make-meta-file (piece path cache meta)
  "Create metadata file for PIECE at PATH using CACHE and META."
  (when-let ((meta (-concat (brb-common-metadata piece cache) meta))
             (meta-file (concat path ".metadata")))
    (with-current-buffer (find-file-noselect meta-file)
      (delete-region (point-min) (point-max))
      (cl-loop for (key value) on meta by 'cddr
               do (insert key ": " "\"" (string-from value) "\"" "\n"))
      (save-buffer))))



(cl-defun brb-copy-images (piece cache)
  "Copy images of a PIECE that is being built to PATH.

CACHE is used to avoid processing images that have not changed."
  (let* ((supported '("jpeg" "png" "jpg" "heic" "webp"))
         (note (plist-get piece :note))
         (deps (porg-cache-get (vulpea-note-id note) :deps cache))
         (path (plist-get piece :target))
         (root (s-chop-suffix (plist-get piece :target-rel) path))
         (dest (let ((name (directory-from-uuid (file-name-base path))))
                 (expand-file-name (concat "images/" name) root)))
         (copied
          (porg-copy-attachments
           note
           :dest-fn dest
           :filter-fn
           (lambda (file) (seq-contains-p supported (s-downcase (file-name-extension file))))
           :copy-fn
           (lambda (file newname &rest _)
             (let* ((dep (-find
                          (lambda (d)
                            (string-equal (file-name-nondirectory file)
                                          (plist-get d :id)))
                          deps))
                    (newname (file-name-fix-attachment newname))
                    (max-width 960)
                    (width (string-to-number
                            (shell-command-to-string
                             (format "identify -format %%W '%s'" file))))
                    ;; if file exists and hash of the source is equal to the cached hash, then we have nothing to do
                    (hash-source (porg-sha1sum file))
                    (hash-cached (when (and (file-exists-p newname) dep)
                                   (plist-get dep :hash))))
               (unless (string-equal hash-source hash-cached)
                 (if (> width max-width)
                     (shell-command-to-string
                      (format "convert '%s' -strip -auto-orient -resize %sx100^ '%s'" file max-width newname))
                   (shell-command-to-string
                    (format "convert '%s' -strip -auto-orient '%s'" file newname))))
               newname)))))
    (when (file-exists-p dest)
      (->> (directory-files dest 'full "[^.]")
           (--remove (-contains-p copied it))
           (--map (delete-file it 'trash))))))



(cl-defun brb-sanitize-id-link (input link)
  "Sanitize ID LINK based on INPUT."
  (if-let* ((id (org-ml-get-property :path link))
            (note (vulpea-db-get-by-id id))
            (piece (gethash id input))
            (file (plist-get piece :target-rel))
            (path (concat "/" (s-chop-suffix ".org" file))))
      (->> link
           (org-ml-set-property :type "barberry")
           (org-ml-set-property :path path))
    (org-ml-from-string
     'plain-text
     (concat (nth 2 link) (s-repeat (or (org-ml-get-property :post-blank link) 0) " ")))))

(cl-defun brb-attached-dependencies (note)
  "Get attached dependencies from NOTE."
  (vulpea-utils-with-note note
    (let ((root (org-attach-dir)))
      (->> (org-element-map (org-element-parse-buffer) 'link #'identity)
           (--filter
            (string-equal "attachment" (or (org-ml-get-property :type it) "not-attachment")))
           (--map (expand-file-name (org-ml-get-property :path it) root))))))



(cl-defun brb-delete (id cache root)
  "Delete item with ID located somewhere in the ROOT.

Hopefully CACHE is useful."
  (let* ((data (gethash id cache))
         (path (plist-get data :target-rel))
         (file (expand-file-name path root))
         (meta (expand-file-name (concat path ".metadata") root))
         (imgs (expand-file-name
                (concat "images/" (directory-from-uuid (file-name-base path)))
                root)))
    (delete-file file)
    (delete-file meta)
    (delete-directory imgs 'recursive)))



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
  "List wines related to wine NOTE.

Return list of notes."
  (--> note
       (vulpea-note-meta-get it "producer" 'note)
       (brb-wines-by-producer it)
       (--remove (string-equal (vulpea-note-id it) (vulpea-note-id note)) it)))

(defun brb-public-notes (notes input)
  "Exchange list of NOTES to list of pieces based on INPUT.

Basically, keep only public notes."
  (->> notes
       (--map (gethash (vulpea-note-id it) input))
       (-filter #'identity)))



(provide 'build-rules)
;;; build-rules.el ends here
