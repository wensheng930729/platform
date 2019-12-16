package com.bee.platform.dinas.datadriver.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * <p>
 * 采购调价主表
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("采购调价主表返回信息")
@JsonInclude(JsonInclude.Include.ALWAYS)
public class DinasPurchaseAdjustDTO implements Serializable {

    private static final long serialVersionUID = 4405589654082449742L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("合同id")
    private Integer orderId;

    @ApiModelProperty("调价时间")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date adjustDate;

    @ApiModelProperty("调价附件")
    private String url;

    @ApiModelProperty("调价附件列表")
    private List<DinasUrlDTO> urlList;

    @ApiModelProperty("调价明细")
    private List<DinasPurchaseAdjustDetailDTO> adjustDetails;

}
