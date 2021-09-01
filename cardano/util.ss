(export #t)

(import
  :std/text/json :std/net/request)

(def (list->json-string l)
  (json-object->string (list->hash-table l)))

(def (struct->hash-table a-struct) #f)

(def (webserver-get url params: (params []) response-parser: (response-parser (cut identity <>)))
  (response-parser (request-json (http-get url params: params))))

(def (webserver-post url data response-parser: (response-parser (cut identity <>)))
  (response-parser (request-text (http-post url
    data: data
    headers: '(("Content-Type" . "application/json"))))))
