package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @Classname ErpPurchaseGoodsOrderSelectRQ
 * @Description 采购收货单查询请求信息
 * @Date 2019/5/30 10:41
 * @Author xin.huang
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel("采购收货单查询请求信息")
public class ErpPurchaseGoodsOrderSelectRQ implements Serializable {
    private static final long serialVersionUID = 6285453360656902676L;

    @ApiModelProperty("采购公司id")
    private Integer companyId;

    @ApiModelProperty("采购订单号")
    private String relatedOrder;

    @ApiModelProperty("供应商名称")
    private String supplyName;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("入库开始日期")
    private String receiptStartDate;

    @ApiModelProperty("入库结束日期")
    private String receiptEndDate;

    @ApiModelProperty("收货单状态")
    private Integer state;

    @ApiModelProperty("入库单号")
    private String code;

    @ApiModelProperty("当前用户所在企业及子企业id列表")
    private List<Integer> enterpriseIdList;
}
