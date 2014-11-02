package getbuild

import (
	"html/template"
	"net/http"

	"github.com/concourse/atc"
	"github.com/concourse/atc/db"
	"github.com/pivotal-golang/lager"
)

type handler struct {
	logger lager.Logger

	db       db.DB
	configDB db.ConfigDB

	template *template.Template
}

func NewHandler(logger lager.Logger, db db.DB, configDB db.ConfigDB, template *template.Template) http.Handler {
	return &handler{
		logger: logger,

		db:       db,
		configDB: configDB,

		template: template,
	}
}

type TemplateData struct {
	Job    atc.JobConfig
	Builds []db.Build

	Build   db.Build
	Inputs  []db.BuildInput
	Outputs []db.BuildOutput

	Abortable bool
}

func (handler *handler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	jobName := r.FormValue(":job")
	if len(jobName) == 0 {
		w.WriteHeader(http.StatusBadRequest)
		return
	}

	buildName := r.FormValue(":build")
	if len(buildName) == 0 {
		w.WriteHeader(http.StatusBadRequest)
		return
	}

	config, err := handler.configDB.GetConfig()
	if err != nil {
		handler.logger.Error("failed-to-load-config", err)
		w.WriteHeader(http.StatusInternalServerError)
		return
	}

	job, found := config.Jobs.Lookup(jobName)
	if !found {
		w.WriteHeader(http.StatusNotFound)
		return
	}

	log := handler.logger.Session("get-build", lager.Data{
		"job":   job.Name,
		"build": buildName,
	})

	build, err := handler.db.GetJobBuild(jobName, buildName)
	if err != nil {
		log.Error("get-build-failed", err)
		w.WriteHeader(http.StatusInternalServerError)
		return
	}

	inputs, outputs, err := handler.db.GetBuildResources(build.ID)
	if err != nil {
		log.Error("failed-to-get-build-resources", err)
		w.WriteHeader(http.StatusInternalServerError)
		return
	}

	bs, err := handler.db.GetAllJobBuilds(jobName)
	if err != nil {
		log.Error("get-all-builds-failed", err)
		w.WriteHeader(http.StatusInternalServerError)
		return
	}

	var abortable bool
	switch build.Status {
	case db.StatusPending, db.StatusStarted:
		abortable = true
	default:
		abortable = false
	}

	templateData := TemplateData{
		Job:    job,
		Builds: bs,

		Build:     build,
		Inputs:    inputs,
		Outputs:   outputs,
		Abortable: abortable,
	}

	err = handler.template.Execute(w, templateData)
	if err != nil {
		log.Fatal("failed-to-execute-template", err, lager.Data{
			"template-data": templateData,
		})
	}
}
