import request from '../common/request'
import apis from '../common/api'

//密码登录
export async function loginPass(params = {}) {
  return request(
    apis.loginCommon.login.api(params),
    {
      method: apis.loginCommon.login.type,
      body: params
    },
    false
  )
}
//验证码登录
export async function loginCode(params = {}) {
  return request(
    apis.loginCommon.loginCode.api(params),
    {
      method: apis.loginCommon.loginCode.type,
      body: params
    },
    true
  )
}
//获取登录验证码
export async function getLoginCode(params = {}) {
  return request(
    apis.getCode.getValidateCodeWithHasAccount.api(params),
    {
      method: apis.getCode.getValidateCodeWithHasAccount.type,
      body: params
    },
    true
  )
}
//获取个人信息
export async function getPersonInfo() {
  return request(
    apis.user.getSelfInfo.api(),
    {
      method: apis.user.getSelfInfo.type
    },
    false
  )
}
//获取用户企业信息
export async function getPersonEnterprises() {
  return request(
    apis.user.enterprises.api(),
    {
      method: apis.user.enterprises.type
    },
    false
  )
}
//查询用户申请认证还未通过的企业信息
export async function getAuthenticatedList(params) {
  return request(
    apis.user.authenticatedList.api(params.phone),
    {
      method: apis.user.authenticatedList.type
    },
    true
  )
}
//切换企业
export async function changeEnterprise(params) {
  return request(
    apis.user.changeCompany.api(params),
    {
      method: apis.user.changeCompany.type
    },
    false
  )
}

//退出登录
export async function logout(params = {}) {
  return request(
    apis.loginCommon.loginOut.api(),
    {
      method: apis.loginCommon.loginOut.type
    },
    true
  )
}
