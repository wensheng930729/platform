package com.bee.platform.dinas.datadriver.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @author xin.huang
 * @description 销售回款列表查询请求信息
 * @date 2019/8/15
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("销售回款列表查询请求信息")
public class SalePaymentListRQ implements Serializable {
    private static final long serialVersionUID = -1741770823287432380L;

    @ApiModelProperty("公司id")
    private Integer companyId;

    @ApiModelProperty("合同编号")
    private String orderCode;

    @ApiModelProperty("回款单号")
    private String code;

    @ApiModelProperty("订货商id")
    private Integer customerId;

    @ApiModelProperty("订货商名称")
    private String customerName;

    @ApiModelProperty("回款开始日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date receiveDateStart;

    @ApiModelProperty("回款结束日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date receiveDateEnd;
}
