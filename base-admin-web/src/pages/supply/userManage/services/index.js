import request from "@/utils/request";
import api from '../apis';
import {supply} from "@/utils/api";

export async function getUserList(params={}) {
  return request(supply.domain+api.getUserList.api(params));
}

export async function getApplyList(params) {
  return request(supply.domain+api.getApplyList.api(params.url), {
    method: api.getApplyList.type,
    body: params.params
  });
}

export async function deleteApply(id) {
  return request(supply.domain+api.deleteApply.api(id), {
    method: api.deleteApply.type,
  });
}

export async function getRoles() {
  return request(supply.domain+api.getRoles.api(), {
    method: api.getRoles.type,
  });
}

export async function getTeamList() {
  return request(supply.domain+api.getTeamList.api(), {
    method: api.getTeamList.type,
  });
}

export async function getTeam() {
  return request(supply.domain+api.getTeam.api(), {
    method: api.getTeam.type,
  });
}

export async function getUserTeam(id) {
  return request(supply.domain+api.getUserTeam.api(id), {
    method: api.getUserTeam.type,
  });
}

export async function changeRole(params) {
  return request(supply.domain+api.changeRole.api(params), {
    method: api.changeRole.type,
    body: params
  });
}

export async function editTeam(params) {
  return request(supply.domain+api.editTeam.api(params), {
    method: api.editTeam.type,
    body: params
  });
}

export async function addTeam(params) {
  return request(supply.domain+api.addTeam.api(params), {
    method: api.addTeam.type,
    body: params
  });
}

export async function deleteTeam(params) {
  return request(supply.domain+api.deleteTeam.api(params.teamId), {
    method: api.deleteTeam.type,
    body: params
  });
}



