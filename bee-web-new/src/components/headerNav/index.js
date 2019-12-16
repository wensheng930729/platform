import React, { Component } from "react";
import style from "./index.less";
import Link from "umi/link";
import { utils, go } from "common";
const { imgGet, openModal } = utils;
const { tms, trade } = go;

// 传入参数：navMsg 例如： [
//     {
//       key: "home",
//       navTitle: "首页",
//       link: "/"
//     },
//     {
//       key: "supplyChain",
//       navTitle: "领蜂供应链",
//       link: "/supplyChain"
//     }]
//imgMsg例如：{
//   path:'header',路径assets下
//   imgName:'icon',图片文件名
//   type:'png'图片类型不传默认为PNG
// }
//pathName：''此页面的key
//color:字体颜色
//borderColor：可传或者不传
//fontWeight:字体加粗，不传默认加粗

export default class HeaderNav extends Component {
  constructor(props) {
    super(props);
    this.state = {
      now: null
    };
  }
  componentDidMount() {
    this.getTime();
  }
  //获取当天小时数
  getTime = () => {
    const now = new Date().getHours();
    this.setState({
      now
    });
  };
  clickItem = key => {
    this.setState({
      path: key
    });
  };
  render() {
    const { now } = this.state;
    const { pathname } = location
    const path = pathname.split('/')[1]
    const secondPath = pathname.split('/')[2]
    const { imgMsg, navMsg, color, borderColor, fontWeight } = this.props;
    return (
      <div className={style.headerNavWrap}>
        <div className={style.headerNav}>
          <div className={style.headerNavBox}>
            <div>
              {imgMsg ? (
                <img
                  src={imgGet(
                    imgMsg.imgName,
                    imgMsg.path,
                    imgMsg.type ? imgMsg.type : "png"
                  )}
                />
              ) : null}
            </div>
            <ul className={style.navContent}>
              {navMsg && navMsg.length > 0 &&
                navMsg.map(item => {
                  return (
                    <li
                      onClick={this.clickItem.bind(this, item.key)}
                      key={item.key}
                      className={`${path}`}
                    >
                      {
                        item.navTitle === "运输信息"
                          ?
                          <a
                            style={
                              item.key === secondPath
                                ? { ...active, color: color, borderBottom: `2px solid ${borderColor ? borderColor : color}` }
                                : { fontWeight: `${fontWeight ? fontWeight : 400}` }
                            }
                            onClick={
                              tms
                            }
                          >
                            {item.navTitle}
                          </a>
                          :

                          item.navTitle === "线上蜂贸"
                            ?
                            <a
                              style={
                                item.key === secondPath
                                  ? { ...active, color: color, borderBottom: `2px solid ${borderColor ? borderColor : color}` }
                                  : { fontWeight: `${fontWeight ? fontWeight : 400}` }
                              }
                              onClick={
                                trade
                              }
                            >
                              {item.navTitle}
                            </a>
                            :

                            <Link
                              style={
                                item.key === secondPath
                                  ? { ...active, color: color, borderBottom: `2px solid ${borderColor ? borderColor : color}` }
                                  : { fontWeight: `${fontWeight ? fontWeight : 400}` }
                              }
                              onClick={
                                item.navTitle === "仓储信息" ? openModal : null
                              }

                              to={item.link}
                            >
                              {item.navTitle}
                            </Link>
                      }

                    </li>
                  );
                })}
            </ul>
          </div>
        </div>
      </div>
    );
  }
}

const active = {
  display: "inline-block",
  height: "30px",
  color: "rgba(252,187,1,1)",
  borderBottom: "2px solid #FCBB01"
};
