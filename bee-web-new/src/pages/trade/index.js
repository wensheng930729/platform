import React, { Component } from "react";
import { Button, Progress } from "antd";
import { utils, go, openModal } from "common";
import withRouter from "umi/withRouter";
import { connect } from "dva";
import "../../common/styles/layout/trade.less";
import "../../common/styles/theme/trade.less";

const { trade } = go;
const { imgGet } = utils;
@withRouter
@connect(({ index }) => ({
  index
}))
export default class Trade extends Component {
  constructor() {
    super();
    this.state = {
      brainIntro:
        "致力于工业大数据应用，围绕工业生产的供应链、采购、生产、销售、服务等多种环节，提供数据可视化展示、多维数据分析与协同生产等智能数据综合服务，为企业决策提供有力依据，加速推动工业制造的智能化转型升级。",
      productInfo: [
        {
          name: "高效寻源",
          iconSrc: "gaoxiao_icon",
          abstract:
            "打通B2B交易市场，集聚行业众多供应商及产品资源，通过供需双方的在线沟通与约谈，全面扩大寻源半径，提升寻源效率。"
        },
        {
          name: "协同采购",
          iconSrc: "xietong_icon",
          abstract:
            "通过对采购流程、行为的管理和优化，实现单据的批量审批和智能流转，有效提升企业议价能力和协同管理水平，降低采购成本。"
        },
        {
          name: "对接ERP",
          iconSrc: "duijie_icon",
          abstract:
            "提供开放接口，与ERP、OA等内部管理系统进行数据对接，基于内部管理、外部寻源和在线交易等全方位需求，打造全程在线的企业交易体系。"
        },
        {
          name: "在线询价、竞价与招标",
          iconSrc: "jingsai_icon",
          abstract:
            "采用在线询价、竞价与招标等多种采购方式，严格记录、清晰存储每一笔订单信息，解决企业采购繁琐跟踪之痛，确保业务公正化、流程阳光化。"
        }
      ],
      solutionPlan: [
        {
          title: "供应商管理",
          abstract:
            "根据供应商资质、审核记录、考核记录等全面信息，帮助企业构建完善、规范的供应商管理系统和考核体系，以供应商可管、可控、可查为核心，优化采购流程，提高采购运作效率。",
          type: "supplier",
          iconSrc: "supplier_icon"
        },
        {
          title: "云工作台",
          abstract:
            "以低成本、高效率为前提，为企业采购用户提供定制化、场景化的整体解决方案，构建多元化组织管理模式，实现企业多角色的协同共建。",
          type: "cloud",
          iconSrc: "cloud_icon"
        },
        {
          title: "系统集成",
          abstract:
            "云工作台与企业内部管理系统的高效集成，在数据整合互联化的基础上，致力于打通企业在线采购与内部管理系统的数据与流程，实现企业基础数据、业务流程应用和单据报表展现的统一调动与获取。",
          type: "systern",
          iconSrc: "systerm_icon"
        },
        {
          title: "寻源采购",
          abstract:
            "基于企业多元采购需求，打造高适配、精定制的采购集成展示门户，实现求购信息与在线报价的快速匹配，助力企业高效采购寻源。",
          type: "purchase",
          iconSrc: "purchase_icon"
        }
      ],
      currentSolution: {
        title: "供应商管理",
        abstract:
          "根据供应商资质、审核记录、考核记录等全面信息，帮助企业构建完善、规范的供应商管理系统和考核体系，以供应商可管、可控、可查为核心，优化采购流程，提高采购运作效率。",
        type: "supplier",
        iconSrc: "supplier_icon"
      },
      currentSolutionType: "supplier",
      enterpriseValue: [
        {
          name: "关系",
          iconSrc: "relationship",
          abstract:
            "构建企业独立的电子采购平台与优质供应商保持紧密高效的业务协同。"
        },
        {
          name: "阳光",
          iconSrc: "sun",
          abstract: "采购业务流程可追溯，可审计，互联网开放、动态、充分竞争。"
        },
        {
          name: "效益",
          iconSrc: "benefit",
          abstract: "直接成本下降15%+，管理效益提升40%+，资金运用效率提升10%+"
        },
        {
          name: "转型",
          iconSrc: "transform",
          abstract: "从事务型采购向战略型采购转型，有效提升企业核心竞争力"
        }
      ],
      sells: [],
      sellsMock:[
        {
          address: "四川省乐山市",
id: 73,
initiateCompanyName: "乐山金蜜工业卫士服务股份有限公司",
name: "高碳铬铁",
paymentMethod: "签订合同之日三个工作日内支付20%",
price: 7000,
releaseTime: "2018-08-30 17:20:20",
residualTotal: "9.0吨",
sn: "403762702579",
taste: "Cr:46,C:10,Si:9,P:0.04,S:0.06,粒度:10-250",
        },
        {
          address: "四川省乐山市",
id: 76,
initiateCompanyName: "乐山金蜜工业卫士服务股份有限公司",
name: "高碳铬铁",
paymentMethod: "现汇",
price: 7200,
releaseTime: "2018-08-31 09:37:30",
residualTotal: "32.0吨",
sn: "691196609199",
taste: "Cr:46,C:10,Si:7,P:0.04,S:0.06,粒度:10-250",
        },
        {
          address: "四川省乐山市",
id: 80,
initiateCompanyName: "四川明达集团峨边合金有限责任公司",
name: "高碳铬铁",
paymentMethod: "现汇",
price: 7000,
releaseTime: "2018-08-31 10:04:26",
residualTotal: "32.0吨",
sn: "119544776036",
taste: "Cr:54,C:10,Si:1.8,P:0.04,S:0.06,粒度:10-300",
        }
      ],
      buys: [],
      buysMock:[{
        address: "乐山市",
buyCompCount: 0,
buyCount: "100吨",
company: "明达云服科技公司",
id: 466,
releaseTime: "2019-08-06 11:42:20",
remainDay: 3,
timePercentage: "0.9",
title: "矿石",
      },{
        address: "乐山市",
buyCompCount: 0,
buyCount: "600吨",
company: "明达云服科技公司",
id: 467,
releaseTime: "2019-08-06 11:44:10",
remainDay: 9,
timePercentage: "0.67",
title: "焦炭",
      },{
        address: "成都市",
buyCompCount: 0,
buyCount: "650吨",
company: "明达云服科技公司",
id: 468,
releaseTime: "2019-08-06 11:47:38",
remainDay: 10,
timePercentage: "0.64",
title: "木屑",
      }]
    };
  }
  componentDidMount() {
    const { dispatch } = this.props;

    dispatch({
      type: "index/getSellsInfo",
      payload: {
        url: `currentPage=1&pageSize=3&type=0`
      },
      callback: resp => {
        this.setState({
          sells: resp.data,
          ...resp.page
        });
      }
    });

    dispatch({
      type: "index/getBuysInfo",
      payload: {
        url: `currentPage=1&pageSize=3&type=0`
      },
      callback: resp => {
        this.setState({
          buys: resp.data,
          ...resp.page
        });
      }
    });
  }
  ChangeSoluteTab(type) {
    console.info("typetype", type);
    const temp = this.handleDate(type);
    // if(type === "supplier"){
    //
    // }else if(type === "cloud"){
    //   temp = this.state.solutionPlan && this.state.solutionPlan.length > 1 ? this.solutionPlan[1]:{};
    // }else if(type === "systern"){
    //   temp = this.state.solutionPlan && this.state.solutionPlan.length > 2 ? this.solutionPlan[2]:{};
    // }if(type === "purchase"){
    //   temp = this.state.solutionPlan && this.state.solutionPlan.length > 3 ? this.solutionPlan[3]:{};
    // }
    // console.info("temptemp",temp);
    this.setState({
      currentSolution: temp,
      currentSolutionType: type
    });
  }
  turnToTrade = (link)=>{
    localStorage.setItem('tradeType',link)
    trade(link)
  }
  handleDate(type) {
    if (this.state.solutionPlan && this.state.solutionPlan.length > 0) {
      for (let i = 0; i < this.state.solutionPlan.length; i++) {
        if (type === this.state.solutionPlan[i].type) {
          return {
            title: this.state.solutionPlan[i].title,
            abstract: this.state.solutionPlan[i].abstract,
            type: this.state.solutionPlan[i].type,
            iconSrc: this.state.solutionPlan[i].iconSrc
          };
        }
      }
    }
  }
  render() {
    const {
      buys,
      buysMock,
      sells,
      sellsMock,
      brainIntro,
      productInfo,
      currentSolutionType,
      currentSolution,
      enterpriseValue
    } = this.state;
    return (
      <div className="flex-column tradeWrap">
        <div className="tradeBannerWrap">
          <div className="flex-column bannerBaseInfo">
            <div className="flex-column bannerBaseInfoText">
              <div className="bannerTitle">线上蜂贸</div>
              <div className="bannerText">
                无缝对接企业内部资源管理,有效提升供应链流转效率
              </div>
              <div className="bannerLink">
                <a className="linkText" href="#">
                  采购寻源
                </a>
                <span className="split_line">|</span>
                <a className="linkText" href="#">
                  云工作台
                </a>
                <span className="split_line">|</span>
                <a className="linkText" href="#">
                  供应商管理
                </a>
                <span className="split_line">|</span>{" "}
                <a className="linkText" href="#">
                  系统集成
                </a>
              </div>
              <div className="flex-row commentWrap">
                <div className="inquiryModule">
                  <div className="flex-row inquiryTitle">
                    询价信息<a onClick={this.turnToTrade.bind(this,'/options/buy')}>更多</a>
                  </div>
                  <div className="flex-column inquiryList">
                    {
                      buys.length
                      ?
                      null
                      :
                      buysMock.map(item => {
                        return (
                          <div key={item.id} className="flex-column inquiryItem">
                            <div className="inquiryListTitle">{item.title}</div>
                            <div className="flex-row perData">
                              <div className="leftItem">地区：{item.address}</div>
                              <div className="rightItem">
                                采购量：<b>{item.buyCount}</b>
                              </div>
                            </div>
                            <div className="flex-row perData">
                              <div className="leftItem">
                                报价企业：{item.buyCompCount}家
                              </div>
                              <div className="rightItem">
                                发布于：
                                {item.releaseTime &&
                                  item.releaseTime.slice(0, 10)}
                              </div>
                            </div>
                            <div className="flex-row perData">
                              <div className="leftItem">
                                供货企业：{item.company}
                              </div>
                            </div>
                            <div className="flex-row perData">
                              <div className="flex-row leftItem">
                                <span className="progress">
                                  <Progress
                                    strokeColor={{
                                      "0%": "#F0AC19",
                                      "100%": "#F8643C"
                                    }}
                                    percent={item.timePercentage * 100}
                                    showInfo={false}
                                  />
                                </span>
                                <span className="dateTime">
                                  仅剩{item.remainDay}天
                                </span>
                              </div>
                              <div className="rightItem">
                                <a onClick={this.turnToTrade.bind(this,'/options/buy')} className="btnBuy">
                                  立即报价
                                </a>
                              </div>
                            </div>
                          </div>
                        );
                      })
                    }
                    {buys.map(item => {
                      return (
                        <div key={item.id} className="flex-column inquiryItem">
                          <div className="inquiryListTitle">{item.title}</div>
                          <div className="flex-row perData">
                            <div className="leftItem">地区：{item.address}</div>
                            <div className="rightItem">
                              采购量：<b>{item.buyCount}</b>
                            </div>
                          </div>
                          <div className="flex-row perData">
                            <div className="leftItem">
                              报价企业：{item.buyCompCount}家
                            </div>
                            <div className="rightItem">
                              发布于：
                              {item.releaseTime &&
                                item.releaseTime.slice(0, 10)}
                            </div>
                          </div>
                          <div className="flex-row perData">
                            <div className="leftItem">
                              供货企业：{item.company}
                            </div>
                          </div>
                          <div className="flex-row perData">
                            <div className="flex-row leftItem">
                              <span className="progress">
                                <Progress
                                  strokeColor={{
                                    "0%": "#F0AC19",
                                    "100%": "#F8643C"
                                  }}
                                  percent={item.timePercentage * 100}
                                  showInfo={false}
                                />
                              </span>
                              <span className="dateTime">
                                仅剩{item.remainDay}天
                              </span>
                            </div>
                            <div className="rightItem">
                              <a onClick={this.turnToTrade.bind(this,'/options/buy')} className="btnBuy">
                                立即报价
                              </a>
                            </div>
                          </div>
                        </div>
                      );
                    })}
                  </div>
                </div>

                <div className="inquiryModule">
                  <div className="flex-row inquiryTitle">
                    现货信息<a onClick={this.turnToTrade.bind(this,'/options/sell')}>更多</a>
                  </div>
                  <div className="flex-column inquiryList">
                    {
                      sells.length
                      ?
                      null
                      :
                      sellsMock.map(item => {
                        return (
                          <div
                            key={item.id}
                            className="flex-column inquiryItem inquirySellItem"
                          >
                            <div className="inquiryListTitle">{item.name}</div>
  
                            <div className="flex-row perData">
                              <div className="leftItem">
                                属性（品味/规格）：{item.taste}
                              </div>
                            </div>
                            <div className="flex-row perData">
                              <div className="leftItem">地区：{item.address}</div>
                              <div className="rightItem">
                                可供应量：<b>{item.residualTotal}</b>
                              </div>
                            </div>
                            <div className="flex-row perData">
                              <div className="leftItem">
                                付款方式：{item.paymentMethod}
                              </div>
                              <div className="rightItem">
                                发布于：
                                {item.releaseTime &&
                                  item.releaseTime.slice(0, 10)}
                              </div>
                            </div>
                            <div className="flex-row perData">
                              <div className="leftItem">
                                供货企业：{item.initiateCompanyName}
                              </div>
                            </div>
                            <div className="flex-row perData">
                              <div className="flex-row leftItem">
                                <span className="sellPrice">
                                  {item.price}
                                  <span className="sellPriceUnit">元</span>
                                </span>
                              </div>
                              <div className="rightItem">
                                <a onClick={this.turnToTrade.bind(this,'/options/buy')} className="btnBuy">
                                  立即下单
                                </a>
                              </div>
                            </div>
                          </div>
                        );
                      })
                    }
                    {sells.map(item => {
                      return (
                        <div
                          key={item.id}
                          className="flex-column inquiryItem inquirySellItem"
                        >
                          <div className="inquiryListTitle">{item.name}</div>

                          <div className="flex-row perData">
                            <div className="leftItem">
                              属性（品味/规格）：{item.taste}
                            </div>
                          </div>
                          <div className="flex-row perData">
                            <div className="leftItem">地区：{item.address}</div>
                            <div className="rightItem">
                              可供应量：<b>{item.residualTotal}</b>
                            </div>
                          </div>
                          <div className="flex-row perData">
                            <div className="leftItem">
                              付款方式：{item.paymentMethod}
                            </div>
                            <div className="rightItem">
                              发布于：
                              {item.releaseTime &&
                                item.releaseTime.slice(0, 10)}
                            </div>
                          </div>
                          <div className="flex-row perData">
                            <div className="leftItem">
                              供货企业：{item.initiateCompanyName}
                            </div>
                          </div>
                          <div className="flex-row perData">
                            <div className="flex-row leftItem">
                              <span className="sellPrice">
                                {item.price}
                                <span className="sellPriceUnit">元</span>
                              </span>
                            </div>
                            <div className="rightItem">
                              <a onClick={this.turnToTrade.bind(this,'/options/buy')} className="btnBuy">
                                立即下单
                              </a>
                            </div>
                          </div>
                        </div>
                      );
                    })}
                  </div>
                </div>
              </div>

              <div className="flex-column tradeProductContair">
                <div className="flex-column moduleTitle">
                  <p>
                    <span className="codeLine">——</span>我们为您提供哪些服务
                    <span className="codeLine">——</span>
                  </p>
                  <p className="partnerName">Process introduction</p>
                  <p className="productTitIcon">
                    <img
                      className="productIconSize"
                      src={imgGet("product_title_icon", "index")}
                    />
                  </p>
                  <p className="productabstract">
                    一站式采购系统，致力于提供全方位、多维度的企业在线交易服务
                  </p>
                </div>
                {productInfo && productInfo.length > 0 ? (
                  <div className="flex-row tradeFunction">
                    {productInfo.map((productItem, productIndex) => {
                      return (
                        <div
                          className="flex-column tradeinFunctionItem"
                          key={`functionItem${productIndex}`}
                        >
                          <p className="functionIcon">
                            <img
                              className="iconSize"
                              src={imgGet(productItem.iconSrc, "trade")}
                            />
                          </p>
                          <p className="functionName">{productItem.name}</p>
                          <p className="functionAbstract">
                            {productItem.abstract}
                          </p>
                        </div>
                      );
                    })}
                  </div>
                ) : null}
              </div>
            </div>
          </div>
        </div>

        <div className="flex-column solutionPlanWrap">
          <div className="flex-column solutionPlanContair">
            <div className="moduleTitle">
              <p>
                <span className="codeLine">——</span>解决方案
                <span className="codeLine">——</span>
              </p>
              <p className="partnerName">Service Advantage</p>
              <p className="productTitIcon">
                <img
                  className="productIconSize"
                  src={imgGet("product_title_icon", "index")}
                />
              </p>
            </div>
            <div className="flex-row solutionPlanContent">
              <div className="solutionPlanImg">
                <img
                  className="imgSize"
                  src={imgGet("solute_plan_img1", "trade")}
                />
                <div className="showBigPlan">
                  <img
                    className="iconSize"
                    src={imgGet(currentSolution.iconSrc, "trade")}
                  />
                </div>
                <div
                  className={`flex-column radiusSolute topRadius ${
                    currentSolutionType === "systern" ? "activeSolution" : ""
                  }`}
                  onClick={this.ChangeSoluteTab.bind(this, "systern")}
                >
                  <span className="flex-column bottomRadiusText">
                    系统
                    <br />
                    集成{" "}
                  </span>
                </div>
                <div
                  className={`flex-column radiusSolute leftRadius ${
                    currentSolutionType === "purchase" ? "activeSolution" : ""
                  }`}
                  onClick={this.ChangeSoluteTab.bind(this, "purchase")}
                >
                  <span className="flex-column bottomRadiusText">
                    寻源
                    <br />
                    采购
                  </span>
                </div>
                <div
                  className={`flex-column radiusSolute rightRadius ${
                    currentSolutionType === "cloud" ? "activeSolution" : ""
                  }`}
                  onClick={this.ChangeSoluteTab.bind(this, "cloud")}
                >
                  <span className="flex-column bottomRadiusText">
                    云工
                    <br />
                    作台
                  </span>
                </div>
                <div
                  className={`flex-column radiusSolute bottomRadius ${
                    currentSolutionType === "supplier" ? "activeSolution" : ""
                  }`}
                  onClick={this.ChangeSoluteTab.bind(this, "supplier")}
                >
                  <span className="flex-column bottomRadiusText">
                    供应商
                    <br />
                    管理
                  </span>
                </div>
              </div>
              <div className="solutionRightInfo">
                <div className="solutionTitle">{currentSolution.title}</div>
                <div className="solutionInfo">{currentSolution.abstract}</div>
              </div>
            </div>
          </div>
        </div>
        <div className="flex-column tradeValue">
          <div className="flex-column tradeValueContair">
            <div className="moduleTitle">
              <p>
                <span className="codeLine">——</span>我们为企业带来的价值
                <span className="codeLine">——</span>
              </p>
              <p className="partnerName">Service Advantage</p>
              <p className="productTitIcon">
                <img
                  className="productIconSize"
                  src={imgGet("product_title_icon", "index")}
                />
              </p>
              <p className="subTitle">
                多元化管理方式，加速实现企业采购战略性转型
              </p>
            </div>
            {enterpriseValue && enterpriseValue.length > 0 ? (
              <div className="flex-row tradeValueCont">
                {enterpriseValue.map((valueItem, valueIndex) => {
                  return (
                    <div
                      className="flex-column tradeValueItem"
                      key={`value${valueIndex}`}
                    >
                      <p className="valueItemIcon">
                        <img
                          className="iconSize"
                          src={imgGet(valueItem.iconSrc, "trade")}
                        />
                      </p>
                      <p className="valueItemName">{valueItem.name}</p>
                      <p className="valueItemText">{valueItem.abstract}</p>
                    </div>
                  );
                })}
              </div>
            ) : null}
          </div>
        </div>
      </div>
    );
  }
}
