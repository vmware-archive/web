package publichandler

import (
	"net/http"

	"github.com/concourse/web/web"
	"github.com/elazarl/go-bindata-assetfs"
)

func NewHandler() (http.Handler, error) {
	publicFS := &assetfs.AssetFS{
		Asset:     web.Asset,
		AssetDir:  web.AssetDir,
		AssetInfo: web.AssetInfo,
	}

	return CacheNearlyForever(http.FileServer(publicFS)), nil
}
