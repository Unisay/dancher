var isProd = window.location.hostname !== "localhost";
var public_path = isProd ? 'https://dancher.herokuapp.com/' : 'http://localhost:8080/';
var fbAppId = isProd ? '320125848412942' : '472336899813781';

exports.config = {
  title:      'Dancher frontend',
  pathPublic: public_path,
  pathApi:    public_path + "api/",
  fbAppId:    fbAppId
};
