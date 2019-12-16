export default {
  getNewsList: {  //获取新闻列表
    api: (url) => `/api/user/news/category?${url}`,
    type: "GET"
  },
  getDetails: {
    api: (id) => `/api/user/news/${id}`,
    type: "GET"
  }
}