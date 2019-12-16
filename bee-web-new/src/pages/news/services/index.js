import { request } from "../../../common";
import api from '../apis';

export async function getNewsList(url) {
  return request(api.getNewsList.api(url));
}

export async function getDetails(id) {
  return request(api.getDetails.api(id));
}

export async function getPage(id) {
  return request(`?id=${id}`, {
    method: 'get'
  });
}