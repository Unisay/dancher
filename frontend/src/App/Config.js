
const isProd = process.env.NODE_ENV !== 'dev';
const public_path = isProd ? 'https://dancher.herokuapp.com/' : 'http://localhost:8081/';

exports.config = {
  title: 'Dancher frontend',
  public_path: public_path,
  api_path: public_path + "api/"
}
