package com.bee.platform.business.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 *
 * @author junyang.li
 * @since 2019-03-20
 */
@Getter
@Setter
@Accessors(chain = true)
@ToString
@NoArgsConstructor
@ApiModel("首页临时数据返回对象")
public class IndexInitDTO implements Serializable{

    private static final long serialVersionUID = -2877575286751005975L;

    @ApiModelProperty("入驻商家")
    private Integer registerEnterprise;

    @ApiModelProperty("物流发布量")
    private Integer publicRequirement;

    @ApiModelProperty("成交金额")
    private BigDecimal supplyMoney;

    private Integer chainEnterpriseCnt;

    private Integer chainServiceCnt;

    @ApiModelProperty("物流合作企业")
    private Integer logisticalEnterpriseCnt;

    @ApiModelProperty("物流单量")
    private Integer logisticalServiceCnt;

    @ApiModelProperty("完成订单数量")
    private Integer orderFulfill;

    @ApiModelProperty("物流运量")
    private BigDecimal number9;

    @ApiModelProperty("线上峰贸-认证供应商(家)")
    private Integer countCompany;
    
    @ApiModelProperty("线上峰贸-最新采购询价(单)")
    private Integer countVaildInquiry;

    @ApiModelProperty("线上峰贸-报价中的供应商(家)")
    private Integer countVaildQuoted;

    @ApiModelProperty("线上峰贸-成交总额")
    private BigDecimal countMoney;


}
