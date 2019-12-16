package com.bee.platform.datadriver.rq;

import com.bee.platform.datadriver.dto.ErpLogoDTO;
import com.bee.platform.datadriver.dto.ErpProductCheckItemsDTO;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.List;

/**
 * @author liang.li
 * @ClassName ErpProductAddRQ
 * @Description erp产品增加入参
 * @Date 2019-5-28
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel("erp产品增加入参")
public class ErpProductAddRQ implements Serializable {

    private static final long serialVersionUID = -1L;

    @ApiModelProperty("企业id-暂时没用-预留字段根据角色权限可选产品所属公司")
    private Integer enterpriseId;

    @ApiModelProperty("产品编码")
    private String code;

    @ApiModelProperty("产品名称")
    private String name;

    @ApiModelProperty("产品单位")
    private String unit;

    @ApiModelProperty("产品类别")
    @NotNull(message = "产品类别不能为空")
    private Integer category;

    @ApiModelProperty("启用批次，1-启用，0-不启用")
    private Integer enableBatch;

    @ApiModelProperty("状态:1-启用,0-禁用")
    private Integer status;

    @ApiModelProperty("产品logo")
    private List<ErpLogoDTO> logo;

    @ApiModelProperty("产品检测属性")
    private List<ErpProductCheckItemsDTO> checkItems;

    @ApiModelProperty("erp产品批次add入参")
    private List<ErpProductBatchRQ> productBatchList;

}
