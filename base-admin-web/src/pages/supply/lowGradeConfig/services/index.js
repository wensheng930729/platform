import request from "@/utils/request";
import api from '../apis';
import {supply} from "@/utils/api";

export async function getList(params={},currentPage,pageSize) {
  return request(supply.domain+api.getList.api(params.businessMode, params.subjectName,currentPage,pageSize));
}

export async function findUpdateCloumns(businessMode) {
  return request(supply.domain+api.findUpdateCloumns.api(businessMode), {
    method: api.findUpdateCloumns.type,
  });
}

export async function getTriggerType(businessMode) {
  return request(supply.domain+api.getTriggerType.api(businessMode), {
    method: api.getTriggerType.type,
  });
}

export async function findLowGradeConfig(id) {
  return request(supply.domain+api.findLowGradeConfig.api(id), {
    method: api.findLowGradeConfig.type,
  });
}

export async function saveLowGradeConfig(params) {
  return request(supply.domain+api.saveLowGradeConfig.api(), {
    method: api.saveLowGradeConfig.type,
    body: params
  });
}

export async function deleteLowGradeConfig(params) {
  return request(supply.domain+api.deleteLowGradeConfig.api(), {
    method: api.deleteLowGradeConfig.type,
    body: params
  });
}