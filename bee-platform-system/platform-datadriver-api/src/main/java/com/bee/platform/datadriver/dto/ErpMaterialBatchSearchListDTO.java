package com.bee.platform.datadriver.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName ErpReceiptSearchRQ
 * @Description 功能描述
 * @Date 2019/5/29 21:21
 **/

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("料批条件搜索返回信息数据")
@JsonInclude
public class ErpMaterialBatchSearchListDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("产成品名称")
    private String productName;


    @ApiModelProperty("料批名称")
    private String materialBatchName;

    @ApiModelProperty("公司id")
    private Integer companyId;

    @ApiModelProperty("公司名称")
    private String companyName;

    @ApiModelProperty("收款状态（0失效，1生效）")
    private Integer state;

}
