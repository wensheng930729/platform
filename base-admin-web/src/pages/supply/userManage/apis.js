export default {
    getUserList: {  //权限中获取用户列表
        api: ({ keyWord, currentPage, pageSize, orderStage }) =>
            `/supplychainfinance-user/user/list?currentPage=${currentPage || 1}&keyWord=${keyWord || ''}&pageSize=${pageSize || 20}&orderStage=${orderStage || '.aes'}`,
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
    getTeamList: {//查询团队列表
        api: () => `/user/team/list`,
        type: 'GET'
    },
    getRoles: {//获得所有的角色
        api: () => `/supplychainfinance-user/user/roles`,
        type: 'GET'
    },
    getUserTeam: {//查询团队Id和名称的列表
        api: (id) => `/supplychainfinance-user/user/team?userId=${id}`,
        type: 'GET'
    },
    getTeam: {//查询团队Id和名称的列表
        api: () => `/supplychainfinance-user/user/teams`,
        type: 'GET'
    }
    , getTeamList: {//查询团队列表
        api: () => `/supplychainfinance-user/user/team/list`,
        type: 'GET'
    }
    , changeRole: {//更改用户角色
        api: (params) =>params.userId? `/supplychainfinance-user/user/change/role`:`/supplychainfinance-user/user/create`,
        type: 'POST'
    },
    editTeam: {//修改团队名称或备注
        api: () => `/supplychainfinance-user/user/team/edit`,
        type: 'POST'
    },
    addTeam: {//添加团队
        api: () => `/supplychainfinance-user/user/team`,
        type: 'POST'
    },
       deleteTeam: {//删除团队
        api: (teamId) => `/supplychainfinance-user/user/team/delete?teamId=${teamId}`,
        type: 'POST'
    },

}