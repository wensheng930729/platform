package com.bee.platform.datadriver.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @Classname ErpPurchaseGoodsOrderDto
 * @Description 货单详情返回信息
 * @Date 2019/5/29 16:42
 * @Author xin.huang
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel(value = "货单详情返回信息")
public class ErpPurchaseGoodsOrderDTO implements Serializable {
    private static final long serialVersionUID = -6228134136827524325L;

    @ApiModelProperty("单号")
    private String code;

    @ApiModelProperty("货单id")
    private String id;

    @ApiModelProperty("记录日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private String receiptDate;

    @ApiModelProperty("源单id")
    private Integer relatedOrderId;

    @ApiModelProperty("源单号")
    private String relatedOrder;

    @ApiModelProperty("公司名称")
    private String purchaseCompany;

    @ApiModelProperty("备注")
    private String remark;

    @ApiModelProperty("附件地址")
    private String url;

    @ApiModelProperty("库存明细")
    private List<RepoReceiptDetailDTO> repoReceiptDetailList;
}
