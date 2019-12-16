import React, { Component } from 'react'
import 'common/styles/layout/energy.less'
import { utils } from 'common'
const { imgGet,openModal } = utils

export default class index extends Component {
    render() {
        return (
            <div className='energyWrap'>
                <div className='energyContent'>
                    <div className='energyBannerContent'>
                        <div className='energyTitle'>
                        能源管控系统解决方案
                        </div>
                        <div className='energyDesc'>
                            <p>
                            实时提取动态生产数据，直观了解设备运行能效，为工厂能源管理构建能耗监管、技术节能和管理节能等综合体系，提升企业能耗的精细化管理水平
                            </p>
                        </div>
                    </div>
                    <div className='energyBannerDetail'>
                        <div className='energyBannerDetailTitle'>
                            <h3><span className='energyBannerDetailTitleLeft'>——</span>详情和优势<span className='energyBannerDetailTitleRight'>——</span></h3>
                            <h4>Details and advantages</h4>
                            <h5><img className="productIconSize" src={imgGet("product_title_icon", "index")} /></h5>
                        </div>
                        <div className='energyBannerDetailContent'>
                            <p>
                            运用物联网手段，自动采集能源各项数据，实时监测各能耗使用情况；在数据上报金蜜工业云后，对能耗数据进行统计与分析，灵活反映线路损耗情况、设备用电情况，助力企业降低能源消耗、提高能源利用效率，实现节能降耗。
                            </p>
                            <div className='energyBannerDetailContentImg'>
                                <div className='energyBannerDetailContentImgLeft energyBannerDetailContentImgBox'>
                                    <p>在线监测能源消耗全过程、全参数</p>
                                </div>
                                <div className='energyBannerDetailContentImgMiddle energyBannerDetailContentImgBox'>
                                <p>能耗动态监管与诊断，安全预警</p>
                                </div>
                                <div className='energyBannerDetailContentImgRight energyBannerDetailContentImgBox'>
                                <p>降低企业能源考核管理难度</p>
                                </div>
                            </div>
                        </div>
                    </div>
                    <div className='energySolutions'>
                    <h3><span className='energySolutionsTitleLeft'>——</span>能源管控系统解决方案示意图<span className='energySolutionsTitleRight'>——</span></h3>
                            <h4>Schematic plan</h4>
                            <h5><img className="productIconSize" src={imgGet("product_title_icon", "index")} /></h5>
                            <div className='energySolutionsImg'>

                            </div>
                    </div>
                </div>
                
                <div className='energyExample'>
                    <div className='energyExampleContent'>
                    <h3><span className='energyExampleContentTitleLeft'>——</span>能源管控系统应用案例<span className='energyExampleContentTitleRight'>——</span></h3>
                            <h4>Application cases</h4>
                            <h5><img className="productIconSize" src={imgGet("product_title_icon", "index")} /></h5>
                            <div className='energyExampleContentDetail'>
                                <div className='energyExampleContentDetailLeft'>
                                    <h3>项目概述</h3>
                                    <p>
                                    四川某硅业有限公司作为国内较早从事硅冶炼的民营企业之一，年产值达数亿元，电力耗能巨大。为进一步提高电能使用率和企业生产效率，该公司与金蜜工业云进行深度合作，采用蜂创物联的智能电表管理系统，通过智能电表进行数据采集，在数据上报金蜜工业云后，将数据进行统计并分析，灵活反映线路损耗情况、设备用电情况，对电能数据进行智能计量和自动监测，助力企业实现节能降耗。
                                    </p>
                                    <div className='energyAsk' onClick={openModal}>方案咨询</div>
                                </div>
                                <div className='energyExampleContentDetailRight'>

                                </div>
                            </div>
                    </div>
                </div>
            </div>
        )
    }
}
