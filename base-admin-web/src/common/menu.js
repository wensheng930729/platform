/* eslint no-useless-escape:0 */
const reg = /(((^https?:(?:\/\/)?)(?:[-;:&=\+\$,\w]+@)?[A-Za-z0-9.-]+|(?:www.|[-;:&=\+\$,\w]+@)[A-Za-z0-9.-]+)((?:\/[\+~%\/.\w-_]*)?\??(?:[-\+=&;%@.\w_]*)#?(?:[\w]*))?)$/g;

export function isUrl(path) {
  return reg.test(path);
}

const menuData = [
  {
    name: '企业管理',
    icon: 'form',
    path: 'enterprise'
  },{
    name: '资讯管理',
    icon: 'table',
    path: 'news'
  },
  {
    name: '供应链金融',
    icon: 'dollar',
    path: 'supply',
    children:[{
      name: '规则配置',
      path: 'lowGradeConfig'
    },
    {
      name: '风控配置',
      path: 'lowRiskConfig'
    },{
      name: '权限管理',
      path: 'userManage'
    },]
  },
  // 
  // 2.0版本开发
  // {
  //   name: '用户管理',
  //   icon: 'solution',
  //   path: 'form',
  // },{
  //   name: '数据统计',
  //   icon: 'line-chart',
  //   path: 'form',
  // },
];

function formatter(data, parentPath = '/', parentAuthority) {
  return data.map(item => {
    let { path } = item;
    if (!isUrl(path)) {
      path = parentPath + item.path;
    }
    const result = {
      ...item,
      path,
      authority: item.authority || parentAuthority,
    };
    if (item.children) {
      result.children = formatter(item.children, `${parentPath}${item.path}/`, item.authority);
    }
    return result;
  });
}

export const getMenuData = () => formatter(menuData);
