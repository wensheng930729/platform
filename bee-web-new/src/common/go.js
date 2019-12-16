import { message } from 'antd';
import { config, request } from './index';
import fetch from 'dva/fetch';
import router from "umi/router";

//判断登录
const isLogin = (way) => {

    let token = 1;  //0通过 1:未登录 2:已登录 未注册企业
    let time = new Date();
    let loginSuccessNextPageTime = time.getTime();
    sessionStorage.loginSuccessNextPage = `${way}`
    sessionStorage.loginSuccessNextPageTime= loginSuccessNextPageTime
    if (localStorage.sysToken && window.g_app._store.getState().global.login) {
        if (localStorage.getItem('companyName') && localStorage.getItem('username') && window.g_app._store.getState().global.company.length > 0) {
            token = 0;
        } 
        else {
            token = 2;
            // router.push(`/enterpriseEnter`)
            // message.info('请先认证企业或等待审核通过!');
            message.info('请联系管理员认证企业或等待管理员配置完成!');
        }
    }
    if (token === 1) {
        message.info('请先登录')
        router.push(`/perLogin`)
    }
    return token;
}
//中台跳转
export const con = (token) => {
    if (isLogin('con') === 0) {
        // window.location.href = `${config.console}?sysToken=${localStorage.getItem('sysToken')}`;
        location.href = `${config.console}/home?sysToken=${localStorage.getItem('sysToken')||token}`
    }
}
//物流跳转
export const tms = () => {
    if (isLogin('tms') === 0) {
        window.open(`${config.tms}/rest/login/getUserInfo?username=${localStorage.getItem('username')}&password=${localStorage.getItem('sysToken')}`);
    }
}

//供应链跳转
export const supplychain = () => {
    if (isLogin('supplychain') === 0) {
        window.open(`${config.supplychain}/role?sysToken=${localStorage.getItem('sysToken')}`);
    }
}

//贸易跳转
export const trade = (path) => {
    if (isLogin('trade') === 0) {
        const goParams = () => `?userCode=${localStorage.getItem('username')}&sysToken=${localStorage.getItem('sysToken')}&company=${encodeURI(encodeURI(localStorage.getItem('companyName')))}`
        if(path){
            location.href=`${config.trade}${path}${goParams()}`;
        }else{
            location.href=`${config.trade}${goParams()}`;
        }
    }
}


//ERP跳转
export const ERP = () => {
    if (isLogin('ERP') === 0) {
        const erpUrl = () => `${config.erp}/web`
        fetch(`${config.erp}/getSubSysAccessToken?username=${localStorage.getItem('username')}&sysToken=${localStorage.getItem('sysToken')}`).then(res => res.json()).then(res => {
            if (res.code === 0) {
                location.href = `${erpUrl()}/login?token=${res.token}`;
            }
            else {
                message.error(res.message)
                sessionStorage.removeItem('loginSuccessNextPage')
                if(location.pathname === '/perLogin'){
                    router.push('/erp')
                }
            }
        });
    }
}

//OA跳转
export const OA = () => {
    if (isLogin('OA') === 0) {
        window.open(config.oa);
    }
}
//ekp跳转
export const EKP = () => {
    if (isLogin()) {
      window.open(`${config.ekp}?sysToken=${localStorage.getItem('sysToken')}`)
    }
}
  

//物联网跳转
export const ifms = () => {
    if (isLogin('ifms') === 0) {
        const ifmsParams = (token) => `/#/total/total?accessToken=${token}`
        const ifmsGoUrl = (token) => `${config.ifms}${ifmsParams(token)}`
        const data = {
            "userCode": localStorage.getItem('username'),
            "password": localStorage.getItem('sysToken')
        };
        request(`${config.ifms}/bee-ifms/login/accessToken`, {
            method: 'POST',
            headers: { 'Accept': 'application/json', "Content-Type": "application/json", 'sysToken': localStorage.getItem('sysToken'), },
            body: data,
        }).then(res => {
            if (res.code === 200) {
                location.href = `${ifmsGoUrl(res.data.accessToken)}`;
            }else{
                message.error(res.message)
                sessionStorage.removeItem('loginSuccessNextPage')
                if(location.pathname === '/perLogin'){
                    router.push('/ifms')
                }
            }
        })
    }
}