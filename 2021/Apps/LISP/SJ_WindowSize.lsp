;;; =========================================================================
;;; File      : WindowSize.lsp
;;; Purpose   : Sets either application window or drawing window size in
;;;             AutoCAD or BricsCAD
;;; Version   : 1.0
;;; Date      : 10 February 2018
;;; Author    : Steve Johnson
;;; Copyright : 2018, cad nauseam Pty Ltd. May be freely used, copied and
;;;             distributed while retaining this notice.
;;; Web       : www.cadnauseam.com
;;; =========================================================================

(vl-load-com)

(defun C:WindowSize (/ app-or-doc acad-obj obj-to-size width height)
  (if (not *cnWS-WIDTH*) (setq *cnWS-WIDTH* 1280))
  (if (not *cnWS-HEIGHT*) (setq *cnWS-HEIGHT* 720))
  (initget "Application Document")
  (setq
    app-or-doc (getkword "\nWindow to size [Application/Document] <Application>: ")
    acad-obj (vlax-get-acad-object)
    obj-to-size (if (= app-or-doc "Document") (vla-get-activedocument acad-obj) acad-obj)
  )
  (initget 6)
  (if (setq width (getint (strcat "\nWidth in pixels <" (itoa *cnWS-WIDTH*) ">: ")))
    (setq *cnWS-WIDTH* width)
    (setq width *cnWS-WIDTH*)
  )
  (if (setq height (getint (strcat "\nHeight in pixels <" (itoa *cnWS-HEIGHT*) ">: ")))
    (setq *cnWS-HEIGHT* height)
    (setq height *cnWS-HEIGHT*)
  )
  (vla-put-width obj-to-size width)
  (vla-put-height obj-to-size height)
  (princ)
)

(princ "\nWindowSize loaded.")
(princ)
;;;-----BEGIN-SIGNATURE-----
;;; /gcAADCCB/oGCSqGSIb3DQEHAqCCB+swggfnAgEBMQ8wDQYJKoZIhvcNAQELBQAw
;;; CwYJKoZIhvcNAQcBoIIFQzCCBT8wggQnoAMCAQICEQCyNMZT2aa05avqeC3j+F3p
;;; MA0GCSqGSIb3DQEBCwUAMH0xCzAJBgNVBAYTAkdCMRswGQYDVQQIExJHcmVhdGVy
;;; IE1hbmNoZXN0ZXIxEDAOBgNVBAcTB1NhbGZvcmQxGjAYBgNVBAoTEUNPTU9ETyBD
;;; QSBMaW1pdGVkMSMwIQYDVQQDExpDT01PRE8gUlNBIENvZGUgU2lnbmluZyBDQTAe
;;; Fw0xNzEwMDQwMDAwMDBaFw0yMTEwMDQyMzU5NTlaMIGAMQswCQYDVQQGEwJBVTEN
;;; MAsGA1UEEQwENjE1NTELMAkGA1UECAwCV0ExEjAQBgNVBAcMCVdpbGxldHRvbjEV
;;; MBMGA1UECQwMMyBGaWZlIENvdXJ0MRQwEgYDVQQKDAtjYWQgbmF1c2VhbTEUMBIG
;;; A1UEAwwLY2FkIG5hdXNlYW0wggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIB
;;; AQC8wj5c1GNf883ljrUXVJcvL9/tRnIhXF0CZ87tOUkqQAhhIM4wReGw0v78jgIS
;;; praT6KAARfToyeeJEHTA29zEtY/wByuS8pvfHpbe6U70eZlEQRAytQTNPKGv+in1
;;; oMjbOngWh+vOgDpaK1YRDjfEbPBO5uJRrPfVfuRHH+3zNCjTwYuWmC0EeemDN0A/
;;; 9rw8mey4ad2UcR/y55fgtsWhw49ip+jaRyVtJYxLPjZhfegGDqYpuXk7i7Wv639v
;;; gqijs2/m8JE+q1ShePVsWn5aLwfgJV2a/KOHMIXMMi1a0Dfx8rk9ZsMXCGj2VxcI
;;; IOVT2lFWLXKU1UT5w+KT7PZNAgMBAAGjggG0MIIBsDAfBgNVHSMEGDAWgBQpkWD/
;;; ik366/mmarjP+eZLvUnOEjAdBgNVHQ4EFgQU9XaiGkcvFLnUSK1dA4ICektBVY0w
;;; DgYDVR0PAQH/BAQDAgeAMAwGA1UdEwEB/wQCMAAwEwYDVR0lBAwwCgYIKwYBBQUH
;;; AwMwEQYJYIZIAYb4QgEBBAQDAgQQMEYGA1UdIAQ/MD0wOwYMKwYBBAGyMQECAQMC
;;; MCswKQYIKwYBBQUHAgEWHWh0dHBzOi8vc2VjdXJlLmNvbW9kby5uZXQvQ1BTMEMG
;;; A1UdHwQ8MDowOKA2oDSGMmh0dHA6Ly9jcmwuY29tb2RvY2EuY29tL0NPTU9ET1JT
;;; QUNvZGVTaWduaW5nQ0EuY3JsMHQGCCsGAQUFBwEBBGgwZjA+BggrBgEFBQcwAoYy
;;; aHR0cDovL2NydC5jb21vZG9jYS5jb20vQ09NT0RPUlNBQ29kZVNpZ25pbmdDQS5j
;;; cnQwJAYIKwYBBQUHMAGGGGh0dHA6Ly9vY3NwLmNvbW9kb2NhLmNvbTAlBgNVHREE
;;; HjAcgRpjZXJ0c3VwcG9ydEBjYWRuYXVzZWFtLmNvbTANBgkqhkiG9w0BAQsFAAOC
;;; AQEAUwLWCrHV1RaRAvdqdlWHNR/p5dlAm+FFIPll96u/p2Z65kj+h8pm+HvpSkg0
;;; RgTaNT/t6VpEVmjT4USrFxKswBjU57WR32a2m6b3di3U/E9gQgrXi5L2fhaCBjDb
;;; sJEZXxsUkXP1s4B5eWxlUvcQjTX0ZVywyfMGi4rqd7wfKHtub0He1qk/Mpqh3IyX
;;; +XCzSnCg9SP90S/xQAdb6EHZRETXHYRx3XBcvOHtWDQouGai/1TmfPDz/LhelheF
;;; bH2Wi3VwUNgkmbW60kxTV9Oy1HyqTjUIAQVRHbgtnU0qth8DGfH3PFwoFD5m3ENe
;;; G5ukN4lyekVWohHVaEHxlZOxOjGCAnswggJ3AgEBMIGSMH0xCzAJBgNVBAYTAkdC
;;; MRswGQYDVQQIExJHcmVhdGVyIE1hbmNoZXN0ZXIxEDAOBgNVBAcTB1NhbGZvcmQx
;;; GjAYBgNVBAoTEUNPTU9ETyBDQSBMaW1pdGVkMSMwIQYDVQQDExpDT01PRE8gUlNB
;;; IENvZGUgU2lnbmluZyBDQQIRALI0xlPZprTlq+p4LeP4XekwDQYJKoZIhvcNAQEL
;;; BQAwDQYJKoZIhvcNAQEBBQAEggEAlHKNcf0rzrHLx5Ru8Pfzs11h0hozbyQmx0a1
;;; DSNsbl0W3peYaW7eqNYjLYq8omXp/WAItZqO0iJzqO2koJXb9bln5EySFZ6+7csf
;;; bnB31gkc9o/M8YjPdGVjQG0VS96RVf/WtkmGugV2n1Fv4wWXBLA7n410yglqSZh9
;;; NOK2Ya1KFx4trccIHV1oAFN+BCKzSf6J/HdVkmCcy4TEPcrxSzZsi//slm2o9EHl
;;; mwdm6Quhw1wMT8+iRmJNO4ofwuKfBwyE28ZIK4q+zorJPNwiK2o43CmNJViU5SQD
;;; M9ImVtHTTtdAR1Iln+wEtg/4xgwj5KWuxoUJ22OJ/K0A8IcnxqGBujCBtwYDVR0O
;;; MYGvBIGsOAAyADsAMgAvADEAMAAvADIAMAAxADgALwA4AC8AMgA2AC8AMQA3AC8A
;;; TgBhAHQAaQBvAG4AYQBsACAASQBuAHMAdABpAHQAdQB0AGUAIABvAGYAIABTAHQA
;;; YQBuAGQAYQByAGQAcwAgAGEAbgBkACAAVABlAGMAaABuAG8AbABvAGcAeQAgACgA
;;; dABpAG0AZQAtAGEALgBuAGkAcwB0AC4AZwBvAHYAKQAAAA==
;;; -----END-SIGNATURE-----