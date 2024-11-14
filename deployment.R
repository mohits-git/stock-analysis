library(rsconnect)

readRenviron("./.env")

rs_acc_name <- Sys.getenv("RS_ACCOUNT_NAME")
rs_token <- Sys.getenv("RS_TOKEN")
rs_secret <- Sys.getenv("RS_SECRET")

rsconnect::setAccountInfo(name=rs_acc_name,
                          token=rs_token,
                          secret=rs_secret)

deployApp(appDir = "./src", appName = "stock-analysis")