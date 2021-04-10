# Парсинг данных с WDI и открытых данных Рф

library('httr')
library('jsonlite')
library('XML')
library('RCurl')
library('WDI')
library('data.table')

# Индикатор нужных нам данных на WDI
indicator.code <- 'IT.CEL.SETS.P2'

dat <- WDI(indicator = indicator.code, start = 2019, end = 2019)
# Загружаем данные
DF <- data.table(dat)

# Загружаем данные в .csv файл
write.csv(DF, file = './data/IT.CEL.SETS.P2.csv', row.names = F)

# Парсим данные с Портала открытых данных РФ

# API ключ
API.key <- 'cbf49d17c9287d22454d2cb452b46848'
URL.base <- 'http://data.gov.ru/api/'

# Функция для работы с API портала открытых данных РФ
getOpenDataRF <- function(api.params, url.base = URL.base, api.key = API.key){
  par <- paste0(api.params, collapse = '/')
  url <- paste0(url.base, par, '/?access_token=', api.key)
  message(paste0('Загружаем ', url, ' ...'))
  resp <- GET(url)
  fromJSON(content(resp, 'text'))
}

dataset_id <- '2308078236-18dom'

# Задаем параметры и получаем данные
params <- c('dataset', dataset_id)
dataset <- getOpenDataRF(params)

# Количество версий таблицы
params <- c(params, 'version')
versions <- getOpenDataRF(params)

nrow(versions)

# Загружаем последнюю версию в объект doc
mrv <- versions[nrow(versions), 1]
params <- c(params, mrv)
content <- c(params, 'content')
doc <- getOpenDataRF(content)

# Оставляем только те данные в которых присутствует поселок Пурпе
doc <- doc[grep('Белореченск', doc$address), c('number', 'dep_name', 'num_cat',
                                               'data_name', 'address')]

head(doc)

# API ключи для Яндекс Карт
API.key <- '3f55a1ff-16cb-4d5b-9105-b7efee04c3df'
URL.base <- 'https://geocode-maps.yandex.ru/1.x/'

# Функция для работы с API Yandex Карт
getYandexMaps <- function(api.params, url.base = URL.base, api.key = API.key){
  par <- paste0(api.params, collapse = '&')
  url <- paste0(url.base, '?format=xml&apikey=', api.key, par)
  message(paste0('Загружаем ', url, ' ...'))
  doc.ya <- content(GET(url), 'text', encoding = 'UTF-8')
  rootNode <- xmlRoot(xmlTreeParse(doc.ya, useInternalNodes = TRUE))
  coords <- xpathSApply(rootNode, "//*[name()='Envelope']/*", xmlValue)
  coords <- lapply(strsplit(coords, ' '), as.numeric)
  coords <- c((coords[[1]][1] + coords[[2]][1])/2, (coords[[1]][2] + coords[[2]][2])/2)
  names(coords) <-c('lat', 'long')
  coords
}

params <-paste0('&geocode=', gsub(pattern =' ', replacement ='+',
                                  curlEscape(doc$address[1])))


# Координаты
coords <- sapply(as.list(doc$address), function(x){
  params <- paste0('&geocode=', gsub(curlEscape(x), pattern = ' ',
                                     replacement = '+'))
  try(getYandexMaps(params))
})

df.coords <- as.data.frame(t(coords))
colnames(df.coords) <- c('long', 'lat')

#Добавляем координаты в основной фрейм данных
doc <- cbind(doc, df.coords)
# Сохраняем данные в файл
write.csv2(doc, file = './data/Portal_RF.csv', row.names = F)
