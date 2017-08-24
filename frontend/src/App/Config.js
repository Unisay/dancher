
const isProd = process.env.NODE_ENV !== 'dev';
const public_path = isProd ? 'https://dancher.herokuapp.com/' : 'http://localhost:8081/';
const fbAppId = isProd ? '320125848412942' : '472336899813781';

exports.config = {
  title:      'Dancher frontend',
  pathPublic: public_path,
  pathApi:    public_path + "api/",
  fbAppId:    fbAppId
};
