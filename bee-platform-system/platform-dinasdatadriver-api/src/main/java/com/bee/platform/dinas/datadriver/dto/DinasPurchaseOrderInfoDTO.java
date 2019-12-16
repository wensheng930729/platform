package com.bee.platform.dinas.datadriver.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * @ClassName PurchaseOrderInfoDTO
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/5/28$ 17:41$
 * @version 1.0.0
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("采购订单返回信息")
@JsonInclude(JsonInclude.Include.ALWAYS)
public class DinasPurchaseOrderInfoDTO implements Serializable {

    private static final long serialVersionUID = 4107940821845886880L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("合同编号")
    private String code;

    @ApiModelProperty("供应商id")
    private Integer customerId;

    @ApiModelProperty("供应商名称")
    private String customerName;

    @ApiModelProperty("合同附件")
    private String url;

    @ApiModelProperty("合同附件列表")
    private List<DinasUrlDTO> urlList;

    @ApiModelProperty("开始时间")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date contractDate;

    @ApiModelProperty("产品明细")
    private List<DinasPurchaseOrderDetailDTO> detailDtos;

    @ApiModelProperty("调价记录")
    private List<DinasPurchaseAdjustDTO> adjustDtos;
}
