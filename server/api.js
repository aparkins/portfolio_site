var jsonServer = require('json-server')

var server = jsonServer.create()

server.use(jsonServer.defaults())

var router = jsonServer.router('server/db.json')
server.use(router)

console.log('Listening on port 4000...')
server.listen(4000)
