import request from "@/utils/request";
import api from '../apis';
import {supply} from "@/utils/api";

export async function getList(params={},currentPage,pageSize) {
  return request(supply.domain+api.getList.api(params.businessMode, params.subjectName,currentPage,pageSize));
}

export async function saveLowRiskConfig(params) {
  return request(supply.domain+api.saveLowRiskConfig.api(), {
    method: api.saveLowRiskConfig.type,
    body: params
  });
}

export async function deleteLowRiskConfig(params={id:''}) {
  return request(supply.domain+api.deleteLowRiskConfig.api(params.id), {
    method: api.deleteLowRiskConfig.type,
    body: params
  });
}

export async function getSysCodeInfo(params) {
  return request(supply.domain+api.getSysCodeInfo.api(params), {
    method: api.getSysCodeInfo.type,
  });
}

