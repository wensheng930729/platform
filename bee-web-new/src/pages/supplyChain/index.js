import React, { Component } from 'react'
import style from './index.less'
import { Button } from 'antd'
import { openModal, imgGet } from '@/common/utils'
import withRouter from 'umi/withRouter'
import Modal from './components/Modal'

@withRouter
export default class SupplyChain extends Component {
  constructor() {
    super()
    this.state = {
      visible: false
    }
  }

  //委托采购详情按钮
  pBoxClick = () => {
    this.props.history.push('/supplyChain/purchase')
    window.scrollTo(0, 0)
  }

  //委托销售详情按钮
  sBoxClick = () => {
    this.props.history.push('/supplyChain/sell')
    window.scrollTo(0, 0)
  }

  //金融仓储详情按钮
  wBoxClick = () => {
    this.props.history.push('/supplyChain/storage')
    window.scrollTo(0, 0)
  }

  //咨询专家按钮
  handleConsulting = () => {
    openModal()
  }

  backChange = () => {
    this.setState({
      visible: !this.state.visible
    })
  }

  changeVisible = () => {
    if (!localStorage.companyName) {
      openModal()
      return
    }
    this.setState({
      visible: !this.state.visible
    })
  }

  render() {
    return (
      <div className="supplyChain">
        <div className="supplyChain_bannerWrap">
          <div className="iconSize">
            <h2>领蜂供应链</h2>
            <h3>领蜂产融智“链”，产业共赢伙伴。</h3>
            <h4>
              <span>供 / 应 / 链 / 生 / 态 / 缔 / 造 / 者</span>
            </h4>
          </div>
          {/* <img src="https://obs-fe91.obs.cn-south-1.myhuaweicloud.com/95e61564c60b4d7e8faa2388ed8fe8e1.png" /> */}
        </div>
        <div className="supplyChain_wrap">
          <div className="supplyChain_content">
            <div className="supplyChain_into">
              <div className="into_title">
                <span className="decorate" />
                <span className="title">领蜂供应链</span>
                <span className="decorate" />
              </div>
              <div className="into_title_bot">SUPPLY CHAIN</div>
              <div className="into_title_icon">
                <div>
                  <img src={imgGet('icon3', 'supply')} />
                </div>
              </div>
              <div className="into_title_info">
                <div>助力企业提升生态系统运转效率，让您的业务鹏程万里</div>
              </div>
              <div className="into_box_wrap">
                <ul>
                  <li className="box">
                    <div>
                      <img src={imgGet('icon2', 'supply')} />
                      <span>委托采购</span>
                    </div>
                    <p>
                      信用赋能，扩大上游供货商销售规模。委托企业只需指定上游供货商并提供保证，便可在采购的过程中享受领蜂供应链提供的“第三方采购”服务，实现实时付款提货。
                    </p>
                  </li>
                  <li className="box">
                    <div>
                      <img src={imgGet('icon1', 'supply')} />
                      <span>委托销售</span>
                    </div>
                    <p>
                      提高双方交易效率，稳定下游购货渠道。当企业的销售面临回款压力时，可通过领蜂供应链提供的“定向销售”服务出售您的货权，让销售货款即刻回笼。
                    </p>
                  </li>
                  <li className="box">
                    <div>
                      <img src={imgGet('icon', 'supply')} />
                      <span>金融仓储</span>
                    </div>
                    <p>
                      激发存货活力，破解融资瓶颈。融资企业将货物存放在领蜂供应链指定的仓库并进行监管，并将货物质押给由领蜂供应链合作的资金方，资金方按照货物价值的一定比例打折后放款。
                    </p>
                  </li>
                </ul>
              </div>
              <div className="into_btn_wrap" onClick={this.changeVisible}>
                <span>
                  {localStorage.companyName ? '立即委托' : '咨询专家'}
                </span>
              </div>
            </div>
            <div className="supplyChain_flow">
              <div className="into_title">
                <span className="decorate" />
                <span className="title">流程介绍</span>
                <span className="decorate" />
              </div>
              <div className="into_title_bot">PROCESS INTRODUCTION</div>
              <div className="into_title_icon">
                <div>
                  <img src={imgGet('icon3', 'supply')} />
                </div>
              </div>
              <div className="flow_info" />
            </div>
            <div className="supplyChain_service">
              <div className="service_wrap">
                <div className="into_title">
                  <span className="decorate" />
                  <span className="title">服务优势</span>
                  <span className="decorate" />
                </div>
                <div className="into_title_bot">SERVICE ADVANTAGE</div>
                <div className="into_title_icon">
                  <div>
                    <img src={imgGet('icon3', 'supply')} />
                  </div>
                </div>
                <div className="service_box_wrap">
                  <ul>
                    <li className="box">
                      <div>
                        <img src={imgGet('bg1', 'supply')} />
                      </div>
                      <h5>高效快捷</h5>

                      <p>
                        内部专业团队分工明细，根据企业实际情况，高效匹配，快速放款。
                      </p>
                    </li>
                    <li className="box">
                      <div>
                        <img src={imgGet('bg2', 'supply')} />
                      </div>
                      <h5>执行灵活</h5>

                      <p>服务与应用相互衔接，灵活实现多维业务场景高效落地。</p>
                    </li>
                    <li className="box">
                      <div>
                        <img src={imgGet('bg3', 'supply')} />
                      </div>
                      <h5>行业资源丰富</h5>

                      <p>
                        多领域数据综合集成分析，丰富统一行业资源，开放创新供应生态链。
                      </p>
                    </li>
                    <li className="box">
                      <div>
                        <img src={imgGet('bg4', 'supply')} />
                      </div>
                      <h5>资金实力雄厚</h5>

                      <p>
                        与多家银行及金融机构紧密合作，可为客户提供稳定持续的全方位供应链服务。
                      </p>
                    </li>
                  </ul>
                </div>
              </div>
            </div>
          </div>
        </div>
        <Modal
          visible={this.state.visible}
          backChange={this.backChange}
        ></Modal>
      </div>
    )
  }
}

const img = id => {
  return require('assets/supply/' + id + '.png')
}
