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
 * @description 销售合同调价请求信息
 * @date 2019/8/14
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("销售合同调价请求信息")
public class SaleAdjustRQ implements Serializable {
    private static final long serialVersionUID = 3534332722621753761L;

    @ApiModelProperty("调价id")
    private Integer id;

    @ApiModelProperty("销售合同id")
    private Integer orderId;

    @ApiModelProperty("调价函附件")
    private List<DinasUrlRQ> urlList;

    @ApiModelProperty("调价日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date adjustDate;

    @ApiModelProperty("销售合同调价明细请求信息")
    private List<SaleAdjustDetailRQ> saleAdjustDetailList;
}
