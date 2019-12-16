package com.bee.platform.dinas.datadriver.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * @author xin.huang
 * @description 销售合同详情返回信息
 * @date 2019/8/14
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("销售合同详情返回信息")
public class SaleOrderDTO implements Serializable {
    private static final long serialVersionUID = -3940186929626977243L;

    @ApiModelProperty("合同id")
    private Integer id;

    @ApiModelProperty("合同编号")
    private String code;

    @ApiModelProperty("订货商id")
    private Integer customerId;

    @ApiModelProperty("订货商名称")
    private String customerName;

    @ApiModelProperty("合同附件")
    private String url;

    @ApiModelProperty("合同附件列表")
    private List<DinasUrlDTO> urlList;

    @ApiModelProperty("合同日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date contractDate;

    @ApiModelProperty("销售合同明细详情返回信息")
    private List<SaleOrderDetailDTO> saleOrderDetailList;

    @ApiModelProperty("销售合同调价函返回信息")
    private List<SaleAdjustDTO> saleAdjustList;

}
