import request from "@/common/request";
import apis from "./api.js"
import { utils } from "common"

export function getSubjectMatterList(params) {
  return request(apis.getSubjectMatterList.api(params), {
    method: apis.getSubjectMatterList.type,
  })
}
export function postBuyApplication(params) {
  return request(apis.postBuyApplication.api(params), {
    method: apis.postBuyApplication.type,
    body: JSON.stringify(params),
  })
}
export function postLargeBuyApplication(params) {
  return request(apis.postLargeBuyApplication.api(params), {
    method: apis.postLargeBuyApplication.type,
    body: JSON.stringify(params),
  })
}
export function postSaleApplication(params) {
  return request(apis.postSaleApplication.api(params), {
    method: apis.postSaleApplication.type,
    body: JSON.stringify(params)
  })
}
export function postStorageApplication(params) {
  return request(apis.postStorageApplication.api(params), {
    method: apis.postStorageApplication.type,
    body: JSON.stringify(params)
  })
}