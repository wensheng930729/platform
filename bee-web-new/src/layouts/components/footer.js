import React, { Component } from 'react'
import { Row, Col } from 'antd'
import Link from 'umi/link'
import '../../common/styles/layout/footer.less'
import '../../common/styles/theme/headFoot.less'
import { utils, go } from 'common'
import withRouter from 'umi/withRouter'
import router from 'umi/router'
const { imgGet } = utils
const { trade } = go

@withRouter
export default class Footer extends Component {
  constructor(props) {
    super(props)
    this.state = {
      footerNav: [
        {
          name: '产品中心',
          link: '',
          childList: [
            {
              title: '蜜云智造',
              link: '/brain'
            },
            {
              title: '领蜂供应链',
              link: '/supplyChain'
            },
            {
              title: '蜂客CRM',
              link: '/crm'
            },
            {
              title: '蜂巢进销存',
              link: '/erp'
            }
          ]
        },
        {
          name: '法律条款',
          link: '',
          childList: [
            {
              title: '服务条款',
              link: '/agreement/service'
            },
            {
              title: '隐私声明',
              link: '/agreement/conceal'
            }
          ]
        },
        {
          name: '关于金蜜',
          link: '',
          childList: [
            {
              title: '关于我们',
              link: '/aboutUs'
            }
          ]
        }
      ],
      footerContact: [
        {
          title: '客服热线：028-85350738',
          img: 'contact'
        },
        {
          title: '办公电话：028-85988267',
          img: 'phone'
        },
        {
          title: 'beesrv@foxmail.com',
          img: 'email'
        },
        {
          title: '成都市高新区天泰路112号12层',
          img: 'location'
        }
      ]
    }
  }
  /**
    name: "服务条款",
    link: "/agreement/service",
    childList:[]

    name: "隐私声明",
    link: "/agreement/conceal",
    childList:[]
  **/
  turnTo = link => {
    router.push(`${link}`)
    window.scrollTo(0, 0)
  }
  render() {
    const { footerNav, footerContact } = this.state

    return (
      <div className="flex-column footerContair">
        <div className="flex-column footerWrap">
          <div className="flex-row footerContent">
            <div className="flex3 flex-row footerCompany">
              {footerNav && footerNav.length > 0
                ? footerNav.map((footerItem, footerIndex) => {
                    return (
                      <ul
                        className="footerListLink"
                        key={`footerCompay${footerIndex}`}
                      >
                        <li className="titleName">{footerItem.name}</li>
                        {footerItem.childList && footerItem.childList.length > 0
                          ? footerItem.childList.map(
                              (childItem, childIndex) => {
                                return (
                                  <li key={`companyItem${childIndex}`}>
                                    <a
                                      onClick={this.turnTo.bind(
                                        this,
                                        childItem.link
                                      )}
                                    >
                                      {childItem.title}
                                    </a>
                                  </li>
                                )
                              }
                            )
                          : null}
                      </ul>
                    )
                  })
                : null}
            </div>
            <div className="flex2 flex-row footerContactWrap">
              <div className="flex1 flex-column footerContact">
                {footerContact.map((item, index) => {
                  return (
                    <div
                      key={`contact${index}`}
                      className="flex-row contactItem"
                    >
                      <div className="flex-item contactTitle">{item.title}</div>
                      <div className="contactIconWrap">
                        <img
                          className="contactIcon"
                          src={imgGet(item.img, 'footer')}
                        />
                      </div>
                    </div>
                  )
                })}
              </div>
              <div className="footerCode">
                <img className="codeSize" src={imgGet('code', 'footer')} />
              </div>
            </div>
          </div>
          <div className="flex-row footerCopyrightBox">
            <div className="footerLogo">
              <img className="footerIcon" src={imgGet('sign', 'footer')} />
            </div>
            <div className="flex1 footerCopyRight">
              <span>Copyright © 2019 金蜜工服 All Rights Reserved</span>
              <span>蜀ICP备16029378号-2</span>
            </div>
          </div>
        </div>
      </div>
    )
  }
}
