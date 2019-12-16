package com.bee.platform.dinas.datadriver.rq;

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
 * @description 销售合同请求信息
 * @date 2019/8/13
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("销售合同请求信息")
public class SaleOrderRQ implements Serializable {
    private static final long serialVersionUID = 8902325520862312206L;

    @ApiModelProperty("合同id")
    private Integer id;

    @ApiModelProperty("合同编号")
    private String code;

    @ApiModelProperty("订货商id")
    private Integer customerId;

    @ApiModelProperty("公司id")
    private Integer companyId;

    @ApiModelProperty("公司名称")
    private String companyName;

    @ApiModelProperty("合同日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date contractDate;

    @ApiModelProperty("合同附件")
    private List<DinasUrlRQ> urlList;

    @ApiModelProperty("销售合同明细请求信息")
    private List<SaleOrderDetailRQ> saleOrderDetailList;

    @ApiModelProperty("销售合同调价请求信息")
    private List<SaleAdjustRQ> saleAdjustList;
}
