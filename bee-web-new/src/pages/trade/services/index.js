import { request } from "common";
import api from '../apis';

export async function getAllNumbers(params = {}) {
  return request(api.getAllNumbers.api(params));
}


export async function getSellsInfo(params = {}) {
  return request(api.getSellsInfo.api(params.url), {
    method: api.getSellsInfo.type
  });
}

export async function getBuysInfo(params = {}) {
  return request(api.getBuysInfo.api(params.url), {
    method: api.getBuysInfo.type
  });
}




