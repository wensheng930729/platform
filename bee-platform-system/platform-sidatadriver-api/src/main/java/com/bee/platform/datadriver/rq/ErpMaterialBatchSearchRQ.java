package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

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
@ApiModel("料批搜索请求参数")
public class ErpMaterialBatchSearchRQ implements Serializable {

    private static final long serialVersionUID = 1L;


    @ApiModelProperty("产成品名称")
    private String productName;

    @ApiModelProperty("料批名称")
    private String materialBatchName;

    @ApiModelProperty("公司名称")
    private Integer companyId;

    @ApiModelProperty("收款状态（0失效，1生效）")
    private Integer state;


    private List<Integer> list;

}
