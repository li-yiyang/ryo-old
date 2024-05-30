(in-package :ryo)

#+darwin
(defun %macos-displaying-notifications
    (message &key (title "" title-set?) (subtitle "" subtitle-set?)
               (sound "" sound-set?) (debug nil)
     &allow-other-keys)
  "Send a notification on macOS.

The `debug' should be a stream for script output, or `nil' for no debug.

Reference:
+ Mac Automation Scripting Guide | Displaying notifications
  https://developer.apple.com/library/archive/documentation/LanguagesUtilities/Conceptual/MacAutomationScriptingGuide/DisplayNotifications.html
"
  (macrolet ((para (set? key val)
               `(if ,set? (format nil "~a ~s" ,key ,val) "")))
    (let ((script (format nil
                          "osascript -e 'display notification ~s ~a ~a ~a ~a'"
                          message
                          (if (or title-set? subtitle-set? sound-set?) "with" "")
                          (para title-set? "title" title)
                          (para subtitle-set? "subtitle" subtitle)
                          (para sound-set? "sound name" sound))))
      (when debug (print script debug))
      (uiop:run-program script :ignore-error-status (not debug)))))

(defun %plain-displaying-notifications
    (message &key &allow-other-keys)
  "Just output to `*standard-output*'. "
  (format *standard-output* message))

(defun notify (message &rest args
               &key (title "RYO") (subtitle "notification") sound
               &allow-other-keys)
  "Send a notify with message. "
  (declare (ignorable title subtitle sound))
  (apply #+darwin
         #'%macos-displaying-notifications
         #+(not darwin)
         #'%plain-displaying-notifications
         message args))
