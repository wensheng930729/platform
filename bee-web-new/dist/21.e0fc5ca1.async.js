(window["webpackJsonp"]=window["webpackJsonp"]||[]).push([[21],{Oz8r:function(e,t,a){"use strict";var n=a("7rOj"),l=a("SyOF");Object.defineProperty(t,"__esModule",{value:!0}),t["default"]=void 0,a("DjyN");var i=n(a("NUBc")),c=n(a("M6Zy")),s=n(a("iyqV")),o=n(a("utfd")),r=n(a("o7c0")),u=n(a("sMup")),d=l(a("COVA")),m=a("MuoO"),f=n(a("xIpT"));a("sYyB");var g,h,p,v=a("kLkQ"),w=n(a("wd/R")),y=(g=(0,m.connect)(function(e){var t=e.news;return{news:t}}),g((p=function(e){function t(e){var a;return(0,c["default"])(this,t),a=(0,o["default"])(this,(0,r["default"])(t).call(this,e)),a.getDatas=function(){var e=arguments.length>0&&void 0!==arguments[0]?arguments[0]:a.state.type,t=arguments.length>1&&void 0!==arguments[1]?arguments[1]:a.state.currentPage,n=a.props.dispatch,l=a.state.pageSize;n({type:"news/getNewsList",payload:"type=".concat(e,"&size=").concat(l,"&page=").concat(t),callback:function(e){a.setState({newsList:e.data.content,currentPage:e.data.number,pageSize:e.data.size,totalPages:e.data.totalPages,totalRecords:e.data.totalElements})}})},a.onChange=function(e){var t=a.state.type;a.getDatas(t,e),a.setState({currentPage:e})},a.getSimpleText=function(e){var t=new RegExp("<.+?>","g"),a=e.replace(t,""),n=new RegExp("&nbsp;","g"),l=a.replace(n,"");return l},a.handleDate=function(e){var t;return e||(e=(0,w["default"])((new Date).format("YYYY-MM-DD hh:mm:ss"))),t=e.slice(0,10).split("-"),t},a.renderImg=function(){var e=10*Math.random();return e>0&&e<2?(0,v.imgGet)("newsDefault0","news"):e>2&&e<4?(0,v.imgGet)("newsDefault1","news"):e>4&&e<6?(0,v.imgGet)("newsDefault2","news"):e>6&&e<8?(0,v.imgGet)("newsDefault3","news"):e>8&&e<10?(0,v.imgGet)("newsDefault4","news"):void 0},a.state={type:1,news:[],newsList:[],currentPage:1,pageSize:6,totalPages:1,totalRecords:0,value:0},a}return(0,u["default"])(t,e),(0,s["default"])(t,[{key:"componentDidMount",value:function(){window.addEventListener("scroll",this.handleScroll),this.getDatas(),console.log(10*Math.random())}},{key:"componentWillUnmount",value:function(){window.removeEventListener("scroll",this.handleScroll)}},{key:"navChanged",value:function(e){this.getDatas(e,1),this.setState({type:e,currentPage:1,value:0})}},{key:"pageChange",value:function(e){this.getDatas(void 0,e),this.setState({currentPage:e})}},{key:"render",value:function(){var e=this,t=[{key:1,title:"\u91d1\u871c\u5feb\u8baf",icon:"kuaixun",iconAc:"kuaixunAc"},{key:0,title:"\u6700\u65b0\u6d3b\u52a8",icon:"huodong",iconAc:"huodongAc"},{key:2,title:"\u5206\u6790\u8bc4\u8bba",icon:"pinlun",iconAc:"pinlunAc"},{key:3,title:"\u4eba\u5de5\u667a\u80fd",icon:"zhineng",iconAc:"zhinengAc"},{key:4,title:"\u5176\u5b83\u8d44\u8baf",icon:"zixun",iconAc:"zixunAc"}],a=this.state,n=a.type,l=a.currentPage,c=a.pageSize,s=a.totalPages,o=a.totalRecords,r=(a.total,a.newsList);return d["default"].createElement("div",{className:"news-main"},d["default"].createElement("div",{className:"banner"}),d["default"].createElement("div",{className:"body"},d["default"].createElement("div",{className:"bodyWrapper"},d["default"].createElement("div",{className:"bodyLeft"},d["default"].createElement("div",{className:"xinwentu"}),d["default"].createElement("ul",{className:"nav"},t.map(function(t){return d["default"].createElement("li",{className:n===t.key?"navItemAc":"navItem",key:t.key,onClick:e.navChanged.bind(e,t.key)},d["default"].createElement("img",{src:n===t.key?(0,v.imgGet)(t.iconAc,"news"):(0,v.imgGet)(t.icon,"news"),style:{marginRight:6,width:20,height:18}}),d["default"].createElement("span",{style:{marginLeft:10}},t.title))}))),d["default"].createElement("div",{className:"bodyRight"},d["default"].createElement("div",{className:"rightHead"}),d["default"].createElement("div",{className:"contentWrapper"},r&&r.length>0?r.map(function(t,a){return d["default"].createElement("div",{className:"contentImg",key:a,style:{fontSize:18}},d["default"].createElement("div",{className:"left"},d["default"].createElement("div",{className:"zuo"},d["default"].createElement("p",{style:{fontSize:20,color:"white",paddingLeft:7}},e.handleDate(t.createAt)[2]),d["default"].createElement("p",{style:{fontSize:12,color:"white"}},e.handleDate(t.createAt)[0],".",e.handleDate(t.createAt)[1])),d["default"].createElement("div",{className:"you"},d["default"].createElement(f["default"],{to:"/news/details?id=".concat(t.id),className:"link",style:{fontSize:20,marginBottom:10,color:"white",display:"block"}},t.title),d["default"].createElement("p",{className:"desc",style:{fontSize:14,color:"rgba(158,161,164,1)",overflow:"hidden",height:40}},e.getSimpleText(t.content)))),d["default"].createElement("div",{className:"right"},d["default"].createElement("img",{src:t.img||e.renderImg(),style:{width:193,height:110}})))}):null,d["default"].createElement("div",{style:{float:"right"}},d["default"].createElement(i["default"],{style:{marginTop:80,color:"white",marginBottom:260},showQuickJumper:!0,showSizeChanger:!0,defaultCurrent:1,defaultPageSize:10,current:l,pageSize:c,total:o,onChange:this.onChange.bind(this),showTotal:function(e,t){return"\u5171 ".concat(o," \u6761\u8bb0\u5f55 \u7b2c ").concat(l," / ").concat(s," \u9875")}})))))))}}]),t}(d.Component),h=p))||h);t["default"]=y},sYyB:function(e,t,a){}}]);