
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

var tap = x => console.log( x ) || x;
var focus = f => x => f( x );

var tasker = ( sender, query ) => Task.find( query || {} ).then( sender );

app.get('/', ( req, res ) => res.sendFile( __dirname + "/index.html" ) );

app.get('/task', ( req, res ) => 
    tasker( x => res.send( x ), req.query )
    .catch( err => console.log( err ) || res.sendStatus( 400 ) ) );
app.ws('/task', ( ws, req ) => {
    // TODO: routines?
    // TODO: sort and filter based on priority
    // BUG: JSON.stringify?
    Task.find( {} ).then( x => ws.send( JSON.stringify( x ) ) ).catch( err => console.log( err ) || ws.sendStatus( 400 ) );
    // setInterval( () => tasker( x => ws.send( x ) ).catch( err => console.log( err ) || ws.sendStatus( 400 ) ), 10000 );
    // TODO: messages with ids update tasks. messages without ids create tasks
    ws.on('message', msg => {
        ( new Task( JSON.parse( msg ) ) )
            .save()
            .then( () => 
                Task.find( {} )
                    .then( x => ws.send( JSON.stringify( x ) ) )
                    .catch( err => console.log( err ) || ws.sendStatus( 400 ) ) );
    });
});

app.put('/task', ( req, res ) => Task.findOneAndUpdate( { _id: req.query._id }, req.query ).then( ( err, tasks ) => err ? res.sendStatus( 400 ) : res.sendStatus( 204 ) ) );

app.post('/task', ( req, res ) => ( new Task( _.defaults( req.query, { done: false, body: "" } ) ) ).save().then( err => err ? res.sendStatus( 400 ) : res.sendStatus( 204 ) ) );

app.listen( PORT, () => console.log( `listening on ${PORT}` ) );
