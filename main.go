package main

import (
	"fmt"
	"io"
	"net/http"

	"code.cloudfoundry.org/lager"
	"github.com/concourse/web/manifest"
	"github.com/concourse/web/publichandler"
	"github.com/concourse/web/robotstxt"
	"github.com/concourse/web/web"
)

func main() {

	logger := lager.NewLogger("web")

	handler, err := web.NewHandler(logger)
	if err != nil {
		panic(err)
	}

	publicHandler, err := publichandler.NewHandler()
	if err != nil {
		panic(err)
	}

	manifestHandler := manifest.NewHandler()
	robotsHandler := robotstxt.Handler{}

	client := &http.Client{}

	apiHandler := func(w http.ResponseWriter, r *http.Request) {

		r.RequestURI = ""
		r.URL.Scheme = "http"
		r.URL.Host = "localhost:8080"

		for _, cookie := range r.Cookies() {
			if cookie.Name == "ATC-Authorization" {
				r.Header.Add("Authorization", cookie.Value)
				r.Header.Del("Cookie")
			}
		}

		resp, err := client.Do(r)
		if err != nil {
			fmt.Println(err)
			w.WriteHeader(http.StatusBadRequest)
			return
		}

		defer resp.Body.Close()
		for k, vv := range resp.Header {
			for _, v := range vv {
				w.Header().Add(k, v)
			}
		}

		w.WriteHeader(resp.StatusCode)
		io.Copy(w, resp.Body)
	}

	http.HandleFunc("/api/", apiHandler)
	http.Handle("/public/", publicHandler)
	http.Handle("/manifest.json", manifestHandler)
	http.Handle("/robots.txt", robotsHandler)
	http.Handle("/", handler)

	if err = http.ListenAndServe(":8082", nil); err != nil {
		panic(err)
	}
}
