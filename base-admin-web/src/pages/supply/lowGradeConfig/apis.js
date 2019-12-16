export default {
    getList: {  //查询已配置的低分规则
        api: (businessMode, subjectName,currentPage,pageSize) =>
            `/supplychainfinance-common/lowGradeConfig/list?businessMode=${businessMode||''}&subjectName=${subjectName||''}&currentPage=${currentPage}&pageSize=${pageSize}`,
        type: "GET"
    },
    saveLowGradeConfig: {//保存低分规则配置
        api: () =>
            `/supplychainfinance-common/lowGradeConfig/saveLowGradeConfig`,
        type: "POST"
    },
    deleteLowGradeConfig: {//删除低分规则配置
        api: () =>
            `/supplychainfinance-common/lowGradeConfig/deleteLowGradeConfig`,
        type: "POST"
    },
    getTriggerType: {//低分规则的风控评分类型列表查询
        api: (businessMode) =>
            `/supplychainfinance-common/lowGradeConfig/triggerType/list?businessMode=${businessMode}`,
        type: "GET"
    },
    findUpdateCloumns: {//查询低分规则配置可修改字段
        api: (businessMode) =>
            `/supplychainfinance-common/lowGradeConfig/findUpdateCloumns?businessMode=${businessMode}`,
        type: "GET"
    },
    findLowGradeConfig: {//标的物低分规则配置详细查询
        api: (lowConfigMasterId) =>
            `/supplychainfinance-common/lowGradeConfig/findLowGradeConfig?lowConfigMasterId=${lowConfigMasterId}`,
        type: "GET"
    },
    saveLowGradeConfig: {//保存低分规则配置
        api: () =>
            `/supplychainfinance-common/lowGradeConfig/saveLowGradeConfig`,
        type: "POST"
    },
}