library(rsconnect)

rsconnect::setAccountInfo(name='monicaisabelbr',
                          token='F727C9F5C74D2A4D54C678903BE2DC6D',
                          secret='PwDlG6FwtCLHiHl8Uxt6tZW2rpafrCp+o3Uw4U1N')

rsconnect::deployApp(appDir = "C:/Users/isabe/OneDrive - Universidad Privada del Valle/4TO SEMESTRE/ESTADISTICA IV/Unidad 2/22_Shiny",
                     appName = "22_myapp")


#rsconnect::showLogs(appName = "22_myapp")

