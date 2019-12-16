import { request } from "common";
import api from './apis';

export async function getSelfInfo(params = {}) {
  return request(api.getSelfInfo.api(), {
    headers: {
      ...params
    }
  });
}

export async function getApplyList(params) {
  return request(supply.domain + api.getApplyList.api(params.url), {
    method: api.getApplyList.type,
    body: params.params
  });
}

export async function changeCom(params) {
  return request(api.changeCom.api(params.id), {
    method: api.changeCom.type,
  });
}

export async function getAllIndustry() {
  return request(`http://192.168.3.160:8075/industry/getAllIndustry`, {
    method: 'get'
  });
}

export async function getAllRegion() {
  return request(`http://192.168.3.160:8075/api/user/region/getAllRegion`, {
    method: 'get'
  });
}

export async function applyCheck(name) {
  return request(`http://192.168.3.160:8075/api/enterprise/enterpriseCheck?name=${name}`, {
    method: 'get'
  });
}

export async function comApply(params) {
  console.log(params)
  return request(`http://192.168.3.160:8075/api/enterprise/register`, {
    method: 'POST',
    body:params
  });
}




