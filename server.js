const express = require('express')
const cors = require('cors')
const bodyParser = require('body-parser')
const app = express()

app.use(cors())
app.use(bodyParser.json())

var todos = []

function addTodo(todo) {
    todos.push(todo)
}

function toggleTodo (index) {
    return todos.map(function (td, i) {
        return i == index ? Object.assign(td, {complete : !td.complete}) : td
    })
}

addTodo({
    text: 'Go and get some milk',
    complete: false
})

addTodo({
    text: 'Write some demo code',
    complete: true
})

app.get('/api/get', function (req, res) {
    res.json(todos)
})

app.post('/api/remove', function (req, res) {
    todos = []
    res.json(todos)
})

app.post('/api/add', function (req, res){
    todos.push(req.body)
    res.json(todos);
})

app.post('/api/toggle/:index', function (req, res){
    todos = toggleTodo(req.params.index)
    res.json(todos);
})

app.listen(4000, function () {
    console.log('Example app listening on port 4000!')
})
