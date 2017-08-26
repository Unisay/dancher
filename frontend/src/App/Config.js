var isProd = process.env.NODE_ENV !== 'dev';
var public_path = isProd ? 'https://dancher.herokuapp.com/' : 'http://localhost:8081/';
var fbAppId = isProd ? '320125848412942' : '472336899813781';

exports.config = {
  title:      'Dancher frontend',
  pathPublic: public_path,
  pathApi:    public_path + "api/",
  fbAppId:    fbAppId
};
