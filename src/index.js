// pull in desired CSS/SASS files
require( './scss/main.scss' );

import ports from './ports';

// inject bundled Elm app into div#main
var Elm = require( './elm/Main' );
console.log(process.env.API_URL);

var storageKey = process.env.STORAGE_KEY;
var storage = localStorage.getItem(storageKey);

// full screen the app and inject the env flags
var app = Elm.Elm.Main.init({flags: {
        storage: JSON.parse(storage),
        config: {
            API_URL: process.env.API_URL,
            APP_NAME: process.env.APP_NAME,
            SOCKET_URL: process.env.SOCKET_URL ? process.env.SOCKET_URL : '',
            FOOTER_MESSAGE: process.env.FOOTER_MESSAGE ? process.env.FOOTER_MESSAGE : '',
        }
    }});

ports(app, storageKey);