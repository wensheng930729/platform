export default {
    getList: {  //查询已配置的低分规则
        api: (businessMode, subjectName,currentPage,pageSize) =>
            `/supplychainfinance-common/lowRiskConfig/config/list?businessMode=${businessMode||''}&subjectName=${subjectName||''}&currentPage=${currentPage}&pageSize=${pageSize}`,
        type: "GET"
    },
    saveLowRiskConfig: {//风控配置新增和修改接口
        api: () =>
            `/supplychainfinance-common/lowRiskConfig/save/config`,
        type: "POST"
    },
    deleteLowRiskConfig: {//删除风控配置
        api: (id) =>`/supplychainfinance-common/lowRiskConfig/delete/config?id=${id}`,
        type: "POST"
    },
    getSysCodeInfo:{
        api:(type='HP')=>`/supplychainfinance-common/subjectMatter/getSubjectMatterList`,
        type: "GET"
    }
    
}