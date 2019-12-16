package com.bee.platform.datadriver.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * @ClassName ErpPurchaseOrderDTO
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/6/6$ 17:13$
 * @version 1.0.0
 */

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("采购订单信息")
public class ErpPurchaseOrderDTO implements Serializable {

    private static final long serialVersionUID = 6225506272331283851L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("合同编号")
    private String contractNo;

    @ApiModelProperty("合同签订日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date contractTime;

    @ApiModelProperty("供应商id")
    private Integer supplier;

    @ApiModelProperty("供应商id")
    private String supplyName;

    @ApiModelProperty("公司id")
    private Integer company;

    @ApiModelProperty("公司名称")
    private String companyName;

    @ApiModelProperty("产品")
    private Integer productId;

    @ApiModelProperty("产品")
    private String productName;

    @ApiModelProperty("采购地")
    private String place;

    @ApiModelProperty("采购方式(0 现货 1 期货)")
    private Integer purchaseMethod;

    @ApiModelProperty("合同质量要求")
    private String requirement;

    @ApiModelProperty("备注")
    private String remark;

    @ApiModelProperty("产品数量")
    private BigDecimal num;

    @ApiModelProperty("单位")
    private String unit;

    @ApiModelProperty("创建人id")
    private Integer purchaseUserId;

    @ApiModelProperty("批次")
    private List<ErpProductBatchListDTO> productBatchList;

}
