import fetch from 'dva/fetch';

function obj2String(obj, arr = [], idx = 0) {
  for (let item in obj) {
    arr[idx++] = [item, obj[item]]
  }
  return new URLSearchParams(arr).toString()
}

const isObject = param =>
  Object.prototype.toString.call(param) === '[object Object]';
const isArray = param =>
  Object.prototype.toString.call(param) === '[object Array]';
const formDates = datas => {
  let result = new FormData()
  for (const i in datas) {
    result.append(i, datas[i])
  }
  return result
};

const get = (url, baseUrl, params) => {
  // url处理
  if (isObject(params)) {
    for (let key in params) {
      if (url.indexOf('?') === -1) {
        url += `?${key}=${params[key]}`
      } else {
        url += `&${key}=${params[key]}`
      }
    }
  }
  return fetch(baseUrl + url, {
    method: 'GET',
    mode: 'cors',
    headers: {
      'sysToken': sessionStorage.sysToken,
      subSysClientid: 'admin-v1.0'
    }
  })
    .then(function (response) {
      if (response.status >= 400) {
        throw new Error('Bad response from server')
      }
      return response.json()
    })
    .catch(function (err) {
      return { code: 401, msg: err.message }
    })
}

const put = (url, baseUrl, params) => {
  // url处理
  if (isObject(params)) {
    for (let key in params) {
      if (url.indexOf('?') === -1) {
        url += `?${key}=${params[key]}`
      } else {
        url += `&${key}=${params[key]}`
      }
    }
  }
  return fetch(baseUrl + url, {
    method: 'PUT',
    headers: {
      'Content-Type': 'application/x-www-form-urlencoded',
      'sysToken': sessionStorage.sysToken, subSysClientid: 'admin-v1.0',
    },
    mode: 'cors',
  })
    .then(function (response) {
      if (response.status >= 400) {
        throw new Error('Bad response from server')
      }
      return response.json()
    })
    .catch(function (err) {
      return { code: 401, msg: err.message }
    })
}

const post = (url, baseUrl, params) => {
  return fetch(baseUrl + url, {
    method: 'POST',
    headers: {
      'Accept': 'application/json',
      'Content-Type': 'application/json;charset=UTF-8',
      'sysToken': sessionStorage.sysToken, subSysClientid: 'admin-v1.0',
    },
    mode: 'cors',
    body: JSON.stringify(params)
  }
  )
    .then(function (response) {
      if (response.status >= 400) {
        throw new Error('Bad response from server')
      }
      return response.json()
    })
    .catch(function (err) {
      return { code: 401, context: err.message }
    })
}

const fetchPost = (url, baseUrl, params, type) => {
  let body = {
    method: 'POST',
    mode: 'cors',
    body: params,
  }
  let headers = { subSysClientid: 'admin-v1.0' };
  if (type !== 'login') {
    headers["sysToken"] = sessionStorage.sysToken
  }
  if (type !== 'edit') {
    headers["Content-Type"] = 'application/x-www-form-urlencoded;charset=UTF-8',
      body["headers"] = headers
  } else {
    body["headers"] = headers
  }
  return fetch(baseUrl + url, body)
    .then(function (response) {
      if (response.status >= 400) {
        throw new Error('Bad response from server')
      }
      return response.json()
    })
    .catch(function (err) {
      return { code: 401, context: err.message }
    })
}

export { get, put, post, fetchPost }