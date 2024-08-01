var crypto = require('crypto');

function getTURNCredentials(name, secret) {
	var unixTimeStamp = 1000,
		username = [unixTimeStamp, name].join(':'),
		password,
		hmac = crypto.createHmac('sha1', secret);
	hmac.setEncoding('base64');
	hmac.write(username);
	hmac.end();
	password = hmac.read();
	return {
		username: username,
		password: password
	};
}

console.log(getTURNCredentials('0', 'secret'));
