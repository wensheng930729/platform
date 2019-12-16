import fetch from 'dva/fetch';
import config from './config';
import { queryString } from './utils';
const codeMessage = {
  200: '服务器成功返回请求的数据。',
  201: '新建或修改数据成功。',
  202: '一个请求已经进入后台排队（异步任务）。',
  204: '删除数据成功。',
  400: '发出的请求有错误，服务器没有进行新建或修改数据的操作。',
  401: '用户没有权限（令牌、用户名、密码错误）。',
  403: '用户得到授权，但是访问是被禁止的。',
  404: '发出的请求针对的是不存在的记录，服务器没有进行操作。',
  406: '请求的格式不可得。',
  410: '请求的资源被永久删除，且不会再得到的。',
  422: '当创建一个对象时，发生一个验证错误。',
  500: '服务器发生错误，请检查服务器。',
  502: '网关错误。',
  503: '服务不可用，服务器暂时过载或维护。',
  504: '网关超时。',
};
function checkStatus(response) {
  if (response.status >= 200 && response.status < 300) {
    return response;
  }
  const errortext = codeMessage[response.status] || response.statusText;
  console.log(`请求错误 ${response.status}: ${response.url}`);
  const error = new Error(errortext);
  error.name = response.status;
  error.response = response;
  throw error;
}

/**
 * Requests a URL, returning a promise.
 *
 * @param  {string} url       The URL we want to request
 * @param  {object} [options] The options we want to pass to "fetch"
 * @return {object}           An object containing either "data" or "err"
 */
export default function request(url, options = {}, isFormData = false, isFile = false) {
  //非正式环境添加请求地址端口与模拟mock
  if (url.indexOf('http') === -1) {
    url = config.domain + url;
  }
  const defaultOptions = {
    // credentials: 'include',
    headers: {
      // subSysClientid: 'platform-v3.0',
      'sysToken': localStorage.sysToken
    }
    // mode:'no-cors'
  };
  //保留默认header信息
  if (options.headers) {
    options.headers = { ...defaultOptions.headers, ...options.headers }
  }
  const newOptions = { ...defaultOptions, ...options };
  if (
    newOptions.method === 'POST' ||
    newOptions.method === 'PUT' ||
    newOptions.method === 'DELETE'
  ) {
    if (isFormData) {
      newOptions.body = queryString(newOptions.body);
      newOptions.headers = {
        Accept: 'application/json',
        ...newOptions.headers,
        'Content-Type': "application/x-www-form-urlencoded",
      }
    }
    else if (!(newOptions.body instanceof FormData)) {
      newOptions.headers = {
        Accept: 'application/json',
        'Content-Type': 'application/json; charset=utf-8',
        ...newOptions.headers,
      };
      //是字符串就不字符化
      if (!(typeof newOptions.body === 'string')) {
        newOptions.body = JSON.stringify(newOptions.body)
      }
    }
    else {
      // newOptions.body is FormData
      newOptions.headers = {
        Accept: 'application/json',
        'Content-Type': 'application/json; charset=utf-8',
        ...newOptions.headers,
      };
    }
    if (isFile) {
      newOptions.headers = {
        Accept: 'application/json',
        subSysClientid: 'platform-v3.0',
        'sysToken': localStorage.sysToken
      };
    }
  }
  // console.log(newOptions)
  return fetch(url, newOptions)
    .then(checkStatus)
    .then(response => {
      // if (newOptions.method === 'DELETE' || response.status === 204) {
      //   return response.text();
      // 
      return response.json();
    }).then(res => {
      if (res.code === 1) {
        res.code = 0;
      } else if (res.code === 0) {
        res.code = 1;
      }
      if (res.object) {
        res.data = res.object;
        delete res.object;
      }
      return res
    })
    .catch(e => {
      return { code: 500, message: '服务器异常', msg: '服务器异常' }
    });;
}
