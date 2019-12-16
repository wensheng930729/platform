
const userDatas = {
  info: {
    id: 1,
    name: '普通用户',
    alias: '北京',
    role: 'user',
  },
  routers: ['/list/bookList'],
  menus: [
    {
      name: '产品(一般用户)',
      icon: 'table',
      path: 'list',
      authority: 'user',
      children: [
        {
          name: '书籍列表',
          path: 'bookList',
          authority: 'admin',
          // hideInBreadcrumb: true,
          // hideInMenu: true,
        },
      ],
    }
  ],
  code: 1,
  message: '登录成功!',
  uuid: '0123456789abcdef'
}

const adminDatas = {
  info: {
    id: 1,
    name: '管理员',
    alias: '北京',
    role: 'admin',
  },
  roleInfo: {
    roleId: '1',
    menus: [
      {
        path: '/home',
        icon: 'dashboard',
        name: '总览',
        hideChildrenInMenu: true,
      }, {
        path: '/incoming',
        icon: 'form',
        name: '进件管理',
        hideChildrenInMenu: true,
        routes: [
          {
            path: '/incoming/purchase/step1',
          }, {
            path: '/incoming/purchase/step2',
          }, {
            path: '/incoming/purchase/step3',
          }, {
            path: '/incoming/purchase/step4',
          }
        ],
      }, {
        path: '/approval',
        icon: 'form',
        name: '审批管理',
        routes: [
          {
            path: '/approval/projectapproval',
            name: '立项审批',
            hideChildrenInMenu: true,
            routes: [
              {
                path: '/approval/projectapproval/index',
              }, {
                path: '/approval/projectapproval/handle',
                name: 'info',
                component: './Approval/ProjectApproval/handle',
              },
            ],
          }, {
            path: '/approval/deferred',
            name: '延期提货',
            hideChildrenInMenu: true,
            routes: [
              {
                path: '/approval/deferred/index',
              }
            ],
          }, {
            path: '/approval/contract',
            name: '合同审批',
            hideChildrenInMenu: true,
            routes: [
              {
                path: '/approval/contract/index',
              }, {
                path: '/approval/contract/detail',
              },
            ],
          }, {
            path: '/approval/settlement',
            name: '结算审批',
            hideChildrenInMenu: true,
            routes: [
              {
                path: '/approval/settlement/index',
              }
            ]
          }, {
            path: '/approval/goods',
            name: '提货申请',
            hideChildrenInMenu: true,
            routes: [
              {
                path: '/approval/goods/index',
              }
            ],
          }, {
            path: '/approval/warehouse',
            name: '仓单审批',
            hideChildrenInMenu: true,
            routes: [
              {
                path: '/approval/warehouse/index',
              }
            ],
          }, {
            path: '/approval/warning',
            name: '预警解除审批',
            hideChildrenInMenu: true,
            routes: [
              {
                path: '/approval/warning/index',
              }, {
                path: '/approval/warning/details',
              }
            ],
          }, {
            path: '/approval/bail',
            name: '保证金审批',
            hideChildrenInMenu: true,
            routes: [
              {
                path: '/approval/bail/index',
              }
            ],
          }
        ]
      }, {
        path: '/orderManage',
        icon: 'form',
        name: '订单管理',
        hideChildrenInMenu: true,
        routes: [
          {
            path: '/orderManage/index',
          }, {
            path: '/orderManage/details',
          }, {
            path: '/orderManage/details/childDetails',
          }, {
            path: '/orderManage/contract/detail',
          }, {
            path: '/orderManage/settlement/edit',
          }, {
            path: '/orderManage/warning/detail',
          },
        ]
      }, {
        path: '/riskManage',
        icon: 'warning',
        name: '风控管理',
        hideChildrenInMenu: true,
        routes: [
          {
            path: '/riskManage/index',
          },
          {
            path: '/riskManage/details',
          }
        ]
      }, {
        path: '/riskConfig',
        name: '规则配置',
        hideChildrenInMenu: true,
        routes: [
          {
            path: '/riskConfig',
          },
          {
            path: '/riskConfig/index',
          }, {
            path: '/riskConfig/configure',
          },
        ]
      }, {
        path: '/userManage',
        name: '权限管理',
        hideChildrenInMenu: true,
        routes: [
          {
            path: '/userManage',
          }, {
            path: '/userManage/index',
          }, {
            path: '/userManage/team',
          },
        ]
      }
    ],
  },
  code: 1,
  message: '登录成功!',
  uuid: '9876543210abcdef'
}

export default {
  'post /api/login': function (req, res, next) {
    setTimeout(() => {
      res.send(adminDatas)
    }, 1500)
  },
  'post /api/checkLogin': function (req, res, next) {
    setTimeout(() => {
      res.json((req.body.uuid === '0123456789abcdef') ?
        userDatas : ((req.body.uuid === '9876543210abcdef') ? adminDatas : { status: 'error', message: '登陆已失效，请重新登陆!' }))
    }, 1500)
  },
  'post /api/logout':
  {
    status: 'ok', message: '退出成功!'
  }
}