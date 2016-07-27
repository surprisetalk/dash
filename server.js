
var PORT = 9458;

var _ = require('underscore');
var express = require('express');
var app = express();
var ews = require('express-ws')(app);

var mongoose = require('mongoose');
mongoose.connect('mongodb://localhost');
mongoose.Promise = global.Promise;

var Task = mongoose.model('Task', { 
    done: { type: Boolean, default: false }, 
    body: String, 
    priority: { type: Number, default: 1 }, 
    updated: { type: Date, default: Date.now } });

// TODO
var tasker = ( sender, query ) => Task.find( query || {} ).then( sender );

app.get('/', ( req, res ) => res.sendfile( __dirname + "/index.html" ) );

app.get('/task', ( req, res ) => 
    tasker( x => res.send( x ), req.query )
    .catch( err => console.log( err ) || res.sendStatus( 400 ) ) );
app.ws('/task', ( ws, req ) => {
    // TODO: routines?
    // TODO: sort and filter based on priority
    setInterval( () => tasker( ws.send ).catch( err => console.log( err ) || ws.sendStatus( 400 ) ), 500 );
    // TODO: messages with ids update tasks. messages without ids create tasks
    // ws.on('message', msg => {
    //   JSON.parse( msg );
    // });
});

app.put('/task', ( req, res ) => Task.findOneAndUpdate( { _id: req.query._id }, req.query ).then( ( err, tasks ) => err ? res.sendStatus( 400 ) : res.sendStatus( 204 ) ) );

app.post('/task', ( req, res ) => ( new Task( _.defaults( req.query, { done: false, body: "" } ) ) ).save().then( err => err ? res.sendStatus( 400 ) : res.sendStatus( 204 ) ) );

app.listen( PORT, () => console.log( `listening on ${PORT}` ) );
