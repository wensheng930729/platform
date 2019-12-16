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
 * @description 销售合同调价函返回信息
 * @date 2019/8/14
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("销售合同调价函返回信息")
public class SaleAdjustDTO implements Serializable {
    private static final long serialVersionUID = 4232809523705796359L;

    @ApiModelProperty("调价函id")
    private Integer id;

    @ApiModelProperty("合同id")
    private Integer orderId;

    @ApiModelProperty("调价附件")
    private String url;

    @ApiModelProperty("调价附件列表")
    private List<DinasUrlDTO> urlList;

    @ApiModelProperty("调价日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date adjustDate ;

    @ApiModelProperty("销售合同调价函明细返回信息")
    private List<SaleAdjustDetailDTO> saleAdjustDetailList ;
}
