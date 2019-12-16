export default {
    getSelfInfo: {  //获取当前登录人的信息
        api: () =>`/api/user/getSelfInfo`,
        type: "GET"
    },
    getUpdateCloumn: {//查询低分规则配置可修改字段
        api: (businessMode) =>
            `/supplychainfinance-common/lowGradeConfig/getUpdateCloumn?businessMode=${businessMode}`,
        type: "GET"
    },
    saveLowGradeConfig: {//保存低分规则配置
        api: () => `/supplychainfinance-common/lowGradeConfig/saveLowGradeConfig`,
        type: "POST"
    },
    changeCom: {  //切换企业
        api: (id) => `/api/user/enterprise/${id}`,
        type: "POST"
    },
}