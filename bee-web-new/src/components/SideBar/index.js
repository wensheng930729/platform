import React, { Component, Fragment } from "react";
import { Carousel, Button, Modal, Row, Col, Form, Input, message } from "antd";
import { withRouter } from "react-router";
import { connect } from "react-redux";
// import * as base from "../../public/base";
// import * as BaseUrl from "../../public/baseUrl";
// import fetch from "isomorphic-fetch";
import "./index.less";
import { utils } from "common";
import { ContactModal } from "../../components";

//传入参数：home、supply、ifms、transport、application、news（对应的是导航条的页面其子页面传入同样的参数）
//参数类型：字符串

const { TextArea } = Input;
const { imgGet, openModal } = utils;



class Index extends Component {
  constructor(props) {
    super(props);
    this.state = {
      visible: false,
      hide: false
    };
  }
  componentDidMount() {
    window.addEventListener('scroll', this.handleScroll);
  }

  componentWillUnmount() {
    window.removeEventListener("scroll", this.handleScroll)
  }
  pageScroll() {
    var time = setInterval(() => {
      var sTop = document.documentElement.scrollTop + document.body.scrollTop;
      window.scrollBy(0, -100);
      if (sTop === 0) {
        clearInterval(time);
      }
    }, 10);
  }
  handleScroll = event => {
    //滚动条高度
    let top = document.documentElement.scrollTop || document.body.scrollTop;
    let clientHeight =
      document.documentElement.clientHeight || document.body.clientHeight; //可视区域高度
    let scrollHeight =
      document.documentElement.scrollHeight || document.body.scrollHeight; //滚动内容高度
    if (top>= 900) {
      this.setState({
        hide: true
      });
    } else {
      this.setState({
        hide: false
      });
    }
  };
  render() {
    const { hide } = this.state;
    const {
      location: { pathname },
    } = this.props;
    const pathWay = pathname === "/" ? "home" : pathname.split("/")[1];
    return hide ? (
      <div className='sideWrap'>
        <div className='sideContact'>
          <div className='sideContactSign'>
            <div
              className={[
                `${pathWay}SideContactBox`,
                `sideContactBox`
              ].join(" ")}
            >
              <img
                className='sideResponse'
                src={imgGet("response", "ifms")}
              />
              <img
                className='sideWhiteResponse'
                src={imgGet("whiteResponse", "ifms")}
              />
              <div
                className='sideContactDetail'
                style={pathWay === "erp" ? { height: "360px" } : {}}
              >
                <div
                  className='sideContactWay'
                  style={pathWay === "erp" ? { height: "360px" } : {}}
                >
                  <dl>
                    <dt>
                      <img
                        className='sideCode'
                        src={imgGet("phone", "ifms")}
                      />
                    </dt>
                    <dd>
                      <p style={{ fontWeight: "normal", fontSize: 16 }}>
                        咨询电话
                      </p>
                      {pathWay === "erp" ? (
                        <p style={{ fontWeight: "normal", fontSize: 14 }}>
                          028-85350738-5
                        </p>
                      ) : (
                        <p style={{ fontWeight: "normal", fontSize: 14 }}>
                          028-85988267
                        </p>
                      )}
                    </dd>
                  </dl>
                  {pathWay === "erp" ? (
                    <dl>
                      <dt>
                        <img
                          className='sideCode'
                          src={imgGet("weChat", "ifms")}
                        />
                      </dt>
                      <dd>
                        <p style={{ fontWeight: "normal" }}>微信</p>
                        <p style={{ fontWeight: "normal", fontSize: 14 }}>
                          18048519711
                        </p>
                      </dd>
                    </dl>
                  ) : null}
                  {pathWay === "erp" ? (
                    <dl>
                      <dt>
                        <img
                          className='sideCode'
                          src={imgGet("QQ", "ifms")}
                        />
                      </dt>
                      <dd>
                        <p style={{ fontWeight: "normal", fontSize: 16 }}>QQ</p>
                        <p style={{ fontWeight: "normal", fontSize: 14 }}>
                          1010071144
                        </p>
                      </dd>
                    </dl>
                  ) : null}
                  <dl style={{ cursor: "pointer" }} onClick={openModal}>
                    <dt>
                      <img
                        className='sideCode'
                        src={imgGet("contact", "ifms")}
                      />
                    </dt>
                    <dd>
                      <p style={{ fontWeight: "normal", fontSize: 16 }}>
                        立即咨询
                      </p>
                      <p style={{ fontWeight: "normal", fontSize: 14 }}>
                        我们竭诚为您服务
                      </p>
                    </dd>
                  </dl>
                </div>
              </div>
            </div>
            <div
              className={[
                `${pathWay}SideContactBox`,
                `sideContactBox`
              ].join(" ")}
            >
              <img className='sideCode' src={imgGet("code", "ifms")} />
              <img
                className='sideWhiteCode'
                src={imgGet("whiteCode", "ifms")}
              />
              <div className='sideContactWx'>
                <div className='sideContactWxWay'>
                  <img className='sideWx' src={imgGet("wx", "ifms")} />
                </div>
              </div>
            </div>
            <div
              className={[
                `${pathWay}SideContactBox`,
                `sideContactBox`
              ].join(" ")}
              onClick={this.pageScroll}
            >
              <div />
              <span>TOP</span>
            </div>
          </div>
        </div>
      </div>
    ) : (
      <Fragment />
    );
  }
}
const Wrap = Form.create()(Index);
export default connect()(withRouter(Wrap));
