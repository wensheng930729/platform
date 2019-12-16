import config from '../../common/config'
export default {
    getAllNumbers: {  //获取主页面上的描述数字
        api: () => `/number/getAllNumbers`,
        type: "GET"
    },

    getSellsInfo: {//获取现货信息
        api: (url) => `${config.trade}/beetrade-business/product/getSaleInfoListForPlatform?${url}`,
        type: 'GET'
    },

    getBuysInfo: {//获取询价信息
        api: (url) => `${config.trade}/beetrade-business/product/getInquiryInfoListForPlatform?${url}`,
        type: 'GET'
    },

}