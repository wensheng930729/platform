

export default {

  'get /supplychainfinance-input/processScheduleBuy/getProcessSchedule': function (req, res, next) {
    setTimeout(() => {
      res.send({code:0})
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

