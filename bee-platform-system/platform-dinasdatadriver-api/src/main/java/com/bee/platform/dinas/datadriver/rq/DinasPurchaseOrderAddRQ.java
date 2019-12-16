package com.bee.platform.dinas.datadriver.rq;

import com.bee.platform.dinas.datadriver.dto.DinasPurchaseAdjustDTO;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.Valid;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * @ClassName PurchaseOrderSaveRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/5/28$ 17:41$
 * @version 1.0.0
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("采购订单新增请求信息")
public class DinasPurchaseOrderAddRQ implements Serializable {

    private static final long serialVersionUID = -1580404456061658276L;

    @ApiModelProperty("合同编号")
    @NotEmpty(message = "合同编号不能为空")
    private String code;

    @ApiModelProperty("供应商")
    @NotNull(message = "合同编号不能为空")
    private Integer customerId;

    @ApiModelProperty("合同附件")
    private List<DinasUrlRQ> url;

    @ApiModelProperty("开始时间")
    @NotNull(message = "合同编号不能为空")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date contractDate;

    @ApiModelProperty("产品明细")
    @Valid
    private List<DinasPurchaseOrderDetailRQ> detailRqs;

}
