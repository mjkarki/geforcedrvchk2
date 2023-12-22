;; BSD 3-Clause License
;;
;; Copyright (c) 2018-2019, Matti J. Kärki
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; * Redistributions of source code must retain the above copyright notice, this
;;   list of conditions and the following disclaimer.
;;
;; * Redistributions in binary form must reproduce the above copyright notice,
;;   this list of conditions and the following disclaimer in the documentation
;;   and/or other materials provided with the distribution.
;;
;; * Neither the name of the copyright holder nor the names of its
;;   contributors may be used to endorse or promote products derived from
;;   this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


(include "nativewin.scm")
(include "misctools.scm")
(include "match.scm")

(define VERSION "1.2")
(define HOST "gfwsl.geforce.com")
(define PATH "/services_toolkit/services/com/nvidia/services/AjaxDriverService.php?func=DriverManualLookup&psid=101&pfid=859&osID=57&languageCode=1033&beta=0&isWHQL=1&dltype=-1&sort1=0&numberOfResults=10")
(define PROGRAMFILES (getenv "ProgramFiles"))
(define NVIDIASMIPATH "NVIDIA Corporation\\NVSMI\\nvidia-smi.exe")
(define NVIDIASMI (string-append PROGRAMFILES "\\" NVIDIASMIPATH))
(define WINDIR (getenv "windir"))
(define CMD (string-append WINDIR "\\System32\\" "cmd.exe"))


;; Name:        show-error-message
;; Description: Show error dialog if installed driver version cannot be obtained.
;; Parameters:  nvidiasmi - full path to nvidia-smi.exe
;; Returns:     #f
(define (show-error-message nvidiasmi)
  (message-box (string-append "Unable to get the driver version!"
                              "\n\n"
                              "Can't get information by using the program:"
                              "\n"
                              nvidiasmi
                              "\n\n"
                              "Maybe the GeForce drivers are not installed?")
               "Error!"
               (bitwise-ior MB_OK MB_ICONSTOP))
  #f)


;; Name:        get-program-output 
;; Description: Executes another process and grabs anything from the stdout.
;; Parameters:  path - full path to an executable
;; Returns:     executable output as a string
(define (get-program-output path)
  (with-exception-handler (lambda (e) "")
    (lambda () (call-with-input-process (list path: path show-console: #f) port->string))))


;; Name:        get-installed-version
;; Description: Gets installed driver version.
;; Parameters:  -
;; Returns:     version number as a number or #f
(define (get-installed-version)
  (let ((matchresult (match "Driver Version: (N.N)"
                       (get-program-output NVIDIASMI))))
    (cond
      ((> (string-length matchresult) 0)
       (string->number matchresult))
      (else
       (show-error-message NVIDIASMI)))))


;; Name:        get-driver-info
;; Description: Checks Nvidia web site and fetches the latest available driver version.
;; Parameters:  -
;; Returns:     '(version url) or '(#f #f)
;;              where version is available driver version as a number
;;              and url is a download URL of the driver
(define (get-driver-info)
  (let ((page (winhttp-get HOST PATH #t)))
    (cond
      ((string? page)
       (list (string->number (match "\"Version\" : \"(N.N)\"" page))
             (match "\"DownloadURL\" : \"(*)\"" page)))
      (else
       (list #f #f)))))


;; Name:        ask-for-update 
;; Description: Asks if user wants to download the latest driver version.
;; Parameters:  installed-version - version number from (get-installed-version)
;;              version           - version number from (get-driver-info)
;; Returns:     IDYES or IDNO
(define (ask-for-update installed-version version)
  (message-box (string-append "Current version: "
                              installed-version
                              "\n"
                              "New version: "
                              version)
               "There is a new GeForce driver available"
               (bitwise-ior MB_YESNO MB_ICONEXCLAMATION)))


;; Name:        open-browser
;; Description: Opens the default system browser using a given URL.
;; Parameters:  url - address to open
;; Returns:     -
(define (open-browser url)
  (open-process (list path: CMD
                      arguments: (list "/c" "start" url)
                      show-console: #f)))


;; Name:        check-version
;; Description: Checks installed driver and available driver versions.
;;              If newer version is available, asks the user if they want to download it.
;; Parameters:  -
;; Returns:     -
(define (check-version)
  (let* ((info (get-driver-info))
         (version (car info))
         (dlurl (cadr info))
         (installed-version (get-installed-version)))
    (cond
      ((and version
            installed-version
            (> version installed-version)
            (= IDYES (ask-for-update (number->string installed-version)
                                     (number->string version))))
       (open-browser dlurl)))))


;; Name:        show-version
;; Description: Shows version information (this tool and the installed Nvidia driver).
;; Parameters:  -
;; Returns:     -
(define (show-version)
  (message-box (string-append "Tool version: " VERSION "\n"
                              "Installed driver version: "
                              (number->string (get-installed-version)))
               "Version information"
               (bitwise-ior MB_OK MB_ICONINFORMATION)))


;; Name:        show-help
;; Description: Shows help information if anything except -v is given at the command line.
;; Parameters:  -
;; Returns:     -
(define (show-help)
  (message-box "GeForceDrvChk.exe [-v]\n-v shows version information" "Usage" (bitwise-ior MB_OK MB_ICONQUESTION)))


;; Name:        main
;; Description: Equivalent for C main() function. Starts the show.
;; Parameters:  -
;; Returns:     -
(define (main)
  (cond
    ((= (length (command-line)) 1)
     (check-version))
    ((and (> (length (command-line)) 1)
          (equal? (cadr (command-line)) "-v"))
     (show-version))
    (else
     (show-help))))


(main)

