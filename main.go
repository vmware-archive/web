package main

import (
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

	http.Handle("/public/", publicHandler)
	http.Handle("/manifest.json", manifestHandler)
	http.Handle("/robots.txt", robotsHandler)
	http.Handle("/", handler)

	if err = http.ListenAndServe(":8082", nil); err != nil {
		panic(err)
	}
}
